module MpstkBackend
  ( MpstkResults(..)
  , MpstkBenchResults(..)
  , toMpstkLocalType
  , toMpstkCtx
  , mpstkVerify
  , mpstkBenchmark
  , mpstkCheckSafety
  , mpstkCheckDeadlockFreedom
  , mpstkCheckLivePlus
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List (intercalate, isPrefixOf)
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hFlush, hClose)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.Map.Strict as Map
import Syntax.AST (Label(..), LocalType(..), Participant(..), PayloadType(..), TypeVar(..))

-- | Results from mpstk verification.
--
-- Note: mpstk's @live+@ property is strictly stronger than our native
-- 'Liveness.checkLiveness'.  Our liveness check tracks pending actions at
-- the @(participant, direction, peer)@ level, while @live+@ requires that
-- every individual /message label/ eventually synchronises.  In practice
-- @live+@ ≈ our liveness ∧ safety.
data MpstkResults = MpstkResults
  { mpstkSafe :: !Bool
  , mpstkDeadlockFree :: !Bool
  , mpstkLivePlus :: !Bool
  } deriving (Eq, Show)

-- | Timing results from mpstk's built-in benchmark mode (@-b N -t default@).
--
-- Times are in seconds and exclude JVM startup overhead (mpstk times only the
-- @pbes2bool@ model-checking calls).
data MpstkBenchResults = MpstkBenchResults
  { mpstkBenchSafe :: !Double
  , mpstkBenchDeadlockFree :: !Double
  , mpstkBenchLivePlus :: !Double
  } deriving (Eq, Show)

toMpstkLocalType :: LocalType -> String
toMpstkLocalType LEnd = "end"
toMpstkLocalType (LVar (TypeVar v)) = v
toMpstkLocalType (LRec (TypeVar v) body) =
  "rec(" ++ v ++ ") " ++ toMpstkLocalType body
toMpstkLocalType (LSend (Participant p) branches) =
  p ++ " (+) " ++ renderBranches branches
toMpstkLocalType (LRecv (Participant p) branches) =
  p ++ " & " ++ renderBranches branches
toMpstkLocalType (LPayloadSend (Participant p) pt cont) =
  p ++ " (+) " ++ renderPayloadType pt ++ " . " ++ toMpstkLocalType cont
toMpstkLocalType (LPayloadRecv (Participant p) pt cont) =
  p ++ " & " ++ renderPayloadType pt ++ " . " ++ toMpstkLocalType cont

renderPayloadType :: PayloadType -> String
renderPayloadType PTInt    = "int"
renderPayloadType PTBool   = "bool"
renderPayloadType PTUnit   = "unit"
renderPayloadType PTString = "string"
renderPayloadType PTFloat  = "float"

renderBranches :: NE.NonEmpty (Label, LocalType) -> String
renderBranches branches
  | NE.length branches == 1 =
      let (Label l, cont) = NE.head branches
       in l ++ " . " ++ toMpstkLocalType cont
  | otherwise =
      "{" ++ intercalate ", " (map renderBranch (NE.toList branches)) ++ "}"
  where
    renderBranch (Label l, cont) = l ++ " . " ++ toMpstkLocalType cont

toMpstkCtx :: Map.Map Participant LocalType -> String
toMpstkCtx ctx =
  intercalate ",\n" entries
  where
    entries =
      [ "s[" ++ getParticipant p ++ "]: " ++ toMpstkLocalType lt
      | (p, lt) <- Map.toAscList ctx
      ]

-- | Run mpstk verify on a context. Requires mpstk on PATH.
-- Returns Left with error message if mpstk fails (e.g. JVM stack overflow).
mpstkVerify :: Map.Map Participant LocalType -> IO (Either String MpstkResults)
mpstkVerify ctx =
  withMpstkFile ctx $ \path -> do
    (exitCode, stdout, stderr_) <- readProcessWithExitCode "mpstk" ["verify", path] ""
    case exitCode of
      ExitSuccess -> pure (Right (parseMpstkOutput stdout))
      ExitFailure code -> pure (Left ("mpstk verify exited with code " ++ show code
                                       ++ ": " ++ take 200 stderr_))

-- | Run mpstk verify in benchmark mode with N repetitions, using @-t default@
-- to exclude JVM overhead. Returns median times (in seconds) for each property.
mpstkBenchmark :: Int -> Map.Map Participant LocalType -> IO (Either String MpstkBenchResults)
mpstkBenchmark n ctx =
  withMpstkFile ctx $ \path -> do
    (exitCode, stdout, stderr_) <- readProcessWithExitCode "mpstk" ["verify", "-b", show n, "-t", "default", path] ""
    case exitCode of
      ExitSuccess -> pure (Right (parseMpstkBenchOutput stdout))
      ExitFailure code -> pure (Left ("mpstk benchmark exited with code " ++ show code
                                       ++ ": " ++ take 200 stderr_))

-- | Write a context to a temporary file and run an action with the file path.
withMpstkFile :: Map.Map Participant LocalType -> (FilePath -> IO a) -> IO a
withMpstkFile ctx action = do
  let ctxStr = toMpstkCtx ctx
  withSystemTempFile "mpst.ctx" $ \path handle -> do
    hPutStr handle ctxStr
    hFlush handle
    hClose handle
    action path

-- | Parse mpstk's tab-separated output table (non-benchmark mode).
parseMpstkOutput :: String -> MpstkResults
parseMpstkOutput output =
  case dropWhile isHeaderLine (lines output) of
    (dataLine : _) ->
      let fields = words dataLine
       in case drop 1 fields of  -- drop the file path
            (df : _live : livePlus : _livePP : _nterm : safe : _term : _) ->
              MpstkResults
                { mpstkSafe = safe == "true"
                , mpstkDeadlockFree = df == "true"
                , mpstkLivePlus = livePlus == "true"
                }
            _ -> error ("mpstk: unexpected output format: " ++ output)
    [] -> error ("mpstk: no data line in output: " ++ output)
  where
    isHeaderLine l = "protocol" `isPrefixOf` l || "Legend:" `isPrefixOf` l || " *" `isPrefixOf` l || null l

-- | Parse mpstk's benchmark output (with @-b N@).
--
-- In benchmark mode, values are @time ± dev%@ instead of @true@/@false@.
-- Column order: @df live live+ live++ nterm safe term@.
parseMpstkBenchOutput :: String -> MpstkBenchResults
parseMpstkBenchOutput output =
  case dropWhile isHeaderLine (lines output) of
    (dataLine : _) ->
      let fields = words dataLine
          -- fields: [filepath, df_time, "±", df_dev, live_time, "±", live_dev,
          --          livePlus_time, "±", livePlus_dev, ..., safe_time, "±", safe_dev, ...]
          -- Each property occupies 3 fields: value "±" deviation
       in case drop 1 fields of  -- drop the file path
            (dfTime : _ : _ : _liveTime : _ : _ : livePlusTime : _ : _ : _livePPTime : _ : _ : _ntermTime : _ : _ : safeTime : _) ->
              MpstkBenchResults
                { mpstkBenchSafe = read safeTime
                , mpstkBenchDeadlockFree = read dfTime
                , mpstkBenchLivePlus = read livePlusTime
                }
            _ -> error ("mpstk benchmark: unexpected output format: " ++ output)
    [] -> error ("mpstk benchmark: no data line in output: " ++ output)
  where
    isHeaderLine l = "protocol" `isPrefixOf` l || "Legend:" `isPrefixOf` l || " *" `isPrefixOf` l || null l

mpstkCheckSafety :: Map.Map Participant LocalType -> IO (Either String Bool)
mpstkCheckSafety ctx = fmap mpstkSafe <$> mpstkVerify ctx

mpstkCheckDeadlockFreedom :: Map.Map Participant LocalType -> IO (Either String Bool)
mpstkCheckDeadlockFreedom ctx = fmap mpstkDeadlockFree <$> mpstkVerify ctx

mpstkCheckLivePlus :: Map.Map Participant LocalType -> IO (Either String Bool)
mpstkCheckLivePlus ctx = fmap mpstkLivePlus <$> mpstkVerify ctx
