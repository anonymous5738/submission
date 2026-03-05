module MpstkBackend
  ( MpstkResults(..)
  , toMpstkLocalType
  , toMpstkCtx
  , mpstkVerify
  , mpstkCheckSafety
  , mpstkCheckDeadlockFreedom
  , mpstkCheckLivePlus
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List (intercalate, isPrefixOf)
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hFlush, hClose)
import System.Process (readProcess)
import qualified Data.Map.Strict as Map
import Syntax.AST (Label(..), LocalType(..), Participant(..), TypeVar(..))

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

toMpstkLocalType :: LocalType -> String
toMpstkLocalType LEnd = "end"
toMpstkLocalType (LVar (TypeVar v)) = v
toMpstkLocalType (LRec (TypeVar v) body) =
  "rec(" ++ v ++ ") " ++ toMpstkLocalType body
toMpstkLocalType (LSend (Participant p) branches) =
  p ++ " (+) " ++ renderBranches branches
toMpstkLocalType (LRecv (Participant p) branches) =
  p ++ " & " ++ renderBranches branches

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
mpstkVerify :: Map.Map Participant LocalType -> IO MpstkResults
mpstkVerify ctx = do
  let ctxStr = toMpstkCtx ctx
  withSystemTempFile "mpst.ctx" $ \path handle -> do
    hPutStr handle ctxStr
    hFlush handle
    hClose handle
    output <- readProcess "mpstk" ["verify", path] ""
    pure (parseMpstkOutput output)

-- | Parse mpstk's tab-separated output table.
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

mpstkCheckSafety :: Map.Map Participant LocalType -> IO Bool
mpstkCheckSafety ctx = mpstkSafe <$> mpstkVerify ctx

mpstkCheckDeadlockFreedom :: Map.Map Participant LocalType -> IO Bool
mpstkCheckDeadlockFreedom ctx = mpstkDeadlockFree <$> mpstkVerify ctx

mpstkCheckLivePlus :: Map.Map Participant LocalType -> IO Bool
mpstkCheckLivePlus ctx = mpstkLivePlus <$> mpstkVerify ctx
