{-# LANGUAGE ScopedTypeVariables #-}
module Benchmark.Runner
  ( GlobalBenchResult(..)
  , LocalBenchResult(..)
  , ProjVariantResult(..)
  , AssociationCheckResult(..)
  , runGlobalBenchmarks
  , runLocalBenchmarks
  , runLocalAssociationChecks
  , runSingleGlobalBenchmark
  , runSingleLocalBenchmark
  , runSingleLocalAssociationCheck
  ) where

import Automata
  ( buildContextGraph
  , buildGlobalGraph
  , buildLocalGraph
  , globalGraphToType
  , localGraphToType
  )
import Balanced (checkBalanced)
import Benchmark.Size (globalTypeSize, localTypeSize, contextSize)
import Benchmark.Types (Citation, ParsedGlobalExample(..), ParsedLocalExample(..))
import Control.Concurrent.QSem (QSem, signalQSem, waitQSem)
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, bracket_, evaluate, try)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Word (Word64)
import System.Mem (performGC)
import Liveness (checkLiveness)
import MpstkBackend (MpstkBenchResults(..), MpstkResults(..), mpstkBenchmark, mpstkVerify)
import Project
  ( projectCoinductiveFull
  , projectCoinductivePlain
  , projectInductiveFull
  , projectInductivePlain
  )
import Subtyping (checkContextSubtype)
import Synthesise (synthesise)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

-- | Result of a single projection variant.
data ProjVariantResult = ProjVariantResult
  { pvTime  :: !NominalDiffTime
  , pvAllOk :: !Bool
  }

-- | Benchmark result for a global type example.
data GlobalBenchResult = GlobalBenchResult
  { gbrName            :: String
  , gbrDisplayName     :: String
  , gbrCitation        :: Maybe Citation
  , gbrGlobalSize      :: Int              -- ^ |G| (AST size)
  , gbrNumParticipants :: Int              -- ^ |pt(G)|
  , gbrBalanced        :: Bool             -- ^ Bal(G)
  , gbrBalancedTime    :: NominalDiffTime  -- ^ time for balanced check
  , gbrProjectedSize   :: Maybe Int        -- ^ |Δ_pro| (graph node count)
  , gbrIP              :: Maybe ProjVariantResult
  , gbrIF              :: Maybe ProjVariantResult
  , gbrCP              :: Maybe ProjVariantResult
  , gbrCF              :: Maybe ProjVariantResult
  }

-- | Benchmark result for a local type (context) example.
data LocalBenchResult = LocalBenchResult
  { lbrName            :: String
  , lbrDisplayName     :: String
  , lbrCitation        :: Maybe Citation
  , lbrContextSize     :: Int              -- ^ |Δ|
  , lbrNumParticipants :: Int              -- ^ |dom(Δ)|
  , lbrSafe            :: Maybe Bool       -- ^ safe(Δ)
  , lbrDeadlockFree    :: Maybe Bool       -- ^ deadlock-free(Δ)
  , lbrLive            :: Maybe Bool       -- ^ live(Δ)
  , lbrMpstkTime       :: Maybe NominalDiffTime
  , lbrInferredSize    :: Maybe Int        -- ^ |G_inf| (AST size)
  , lbrInferredBalanced :: Maybe Bool      -- ^ Bal(G_inf)
  , lbrSynthesisTime   :: Maybe NominalDiffTime
  }

-- | Result of checking that a synthesised global type is associated
-- with the original local context by reprojecting and checking
-- pointwise subtyping against the input.
data AssociationCheckResult = AssociationCheckResult
  { acrName         :: String
  , acrDisplayName  :: String
  , acrPassed       :: Bool
  , acrMessage      :: String
  , acrInferredSize :: Maybe Int
  }

-- ---------------------------------------------------------------------------
-- Global benchmarks
-- ---------------------------------------------------------------------------

runGlobalBenchmarks :: Int -> Int -> [ParsedGlobalExample] -> IO [GlobalBenchResult]
runGlobalBenchmarks runs timeoutSecs =
  mapM (runSingleGlobalBenchmark runs timeoutSecs)

runSingleGlobalBenchmark :: Int -> Int -> ParsedGlobalExample -> IO GlobalBenchResult
runSingleGlobalBenchmark runs timeoutSecs ex = do
  let gt = pgeGlobalType ex
      participants = pgeParticipants ex
      benchTimeout = timeoutSecs * 1000000

  hPutStrLn stderr $ "  [Global] " ++ pgeName ex

  let gSize = globalTypeSize gt
      nParticipants = length participants

  -- Build global graph once for boolean results
  let gg = buildGlobalGraph gt
      balanced = checkBalanced gg == Right ()

  -- Balanced check (timed, median over runs)
  balTimes <- sequence
    [ timeIO_ $ do
        gg' <- evaluate $ force $ buildGlobalGraph gt
        _ <- evaluate $ force $ checkBalanced gg'
        pure ()
    | _ <- [1..runs]
    ]
  let balTime = median balTimes

  -- Projected context size (AST size via coinductive full projection)
  let projSize = case sequence [projectCoinductiveFull gg p | p <- participants] of
        Left _   -> Nothing
        Right lgs -> case sequence [localGraphToType lg | lg <- lgs] of
          Left _   -> Nothing
          Right lts -> Just (sum (map localTypeSize lts))

  -- 4 projection variants (timed)
  ip  <- benchVariant benchTimeout runs gt participants projectInductivePlain
  ifu <- benchVariant benchTimeout runs gt participants projectInductiveFull
  cp  <- benchVariant benchTimeout runs gt participants projectCoinductivePlain
  cf  <- benchVariant benchTimeout runs gt participants projectCoinductiveFull

  pure GlobalBenchResult
    { gbrName            = pgeName ex
    , gbrDisplayName     = pgeDisplayName ex
    , gbrCitation        = pgeCitation ex
    , gbrGlobalSize      = gSize
    , gbrNumParticipants = nParticipants
    , gbrBalanced        = balanced
    , gbrBalancedTime    = balTime
    , gbrProjectedSize   = projSize
    , gbrIP              = ip
    , gbrIF              = ifu
    , gbrCP              = cp
    , gbrCF              = cf
    }

-- ---------------------------------------------------------------------------
-- Local benchmarks
-- ---------------------------------------------------------------------------

runLocalBenchmarks ::
  Bool -> Int -> Int -> Maybe QSem ->
  [ParsedLocalExample] -> IO [LocalBenchResult]
runLocalBenchmarks useMpstk runs timeoutSecs mpstkSem =
  mapM (runSingleLocalBenchmark useMpstk runs timeoutSecs mpstkSem)

runLocalAssociationChecks :: [ParsedLocalExample] -> IO [AssociationCheckResult]
runLocalAssociationChecks =
  mapM runSingleLocalAssociationCheck

runSingleLocalBenchmark ::
  Bool -> Int -> Int -> Maybe QSem ->
  ParsedLocalExample -> IO LocalBenchResult
runSingleLocalBenchmark useMpstk runs timeoutSecs mpstkSem ex = do
  let context = pleContext ex
      participantOrder = pleParticipantOrder ex
      benchTimeout = timeoutSecs * 1000000

  hPutStrLn stderr $ "  [Local] " ++ pleName ex

  let ctxSize = contextSize context
      nParticipants = Map.size context

  -- Safety/Liveness/Deadlock-freedom from mpstk (with Haskell fallback)
  (safe, df, live, mpstkTime) <- if useMpstk
    then withMpstkSem mpstkSem $ do
      resultE <- try $ do
        verifyE <- mpstkVerify context
        case verifyE of
          Left err -> do
            hPutStrLn stderr $ "    WARNING: mpstk verify failed: " ++ err
            pure (Nothing, Nothing, Nothing, Nothing)
          Right res -> do
            benchE <- mpstkBenchmark runs context
            let mTime = case benchE of
                  Right bench -> Just $ realToFrac $ maximum
                    [ mpstkBenchSafe bench
                    , mpstkBenchDeadlockFree bench
                    , mpstkBenchLivePlus bench
                    ]
                  Left _ -> Nothing
            -- Determine safe/deadlock-free/live
            let s  = mpstkSafe res
                d  = mpstkDeadlockFree res
                lp = mpstkLivePlus res
            l <- if not s && not lp
              then do
                -- Build context graph for Haskell liveness fallback
                let cg = buildContextGraph
                      [ (p, buildLocalGraph lt)
                      | p <- participantOrder
                      , Just lt <- [Map.lookup p context]
                      ]
                pure $ checkLiveness cg == Right ()
              else pure lp
            pure (Just s, Just d, Just l, mTime)
      case resultE of
        Left (e :: SomeException) -> do
          hPutStrLn stderr $ "    WARNING: mpstk crashed: " ++ show e
          pure (Nothing, Nothing, Nothing, Nothing)
        Right r -> pure r
    else pure (Nothing, Nothing, Nothing, Nothing)

  -- Synthesis (timed, median over runs)
  -- Timing includes: graph building + synthesise + graph→syntax (full round-trip)
  synthResult <- timeout benchTimeout $ do
    results <- sequence
      [ timeIO $ do
             cg' <- evaluate $ force $ buildContextGraph
                      [ (p, buildLocalGraph lt)
                      | p <- participantOrder
                      , Just lt <- [Map.lookup p context]
                      ]
             gg <- evaluate $ force $ synthesise cg'
             case gg of
               Left err -> pure (Left err)
               Right g  -> do
                 _ <- evaluate $ force $ globalGraphToType g
                 pure (Right g)
      | _ <- [1..runs]
      ]
    let times = map fst results
        result = snd (head results)  -- deterministic
    pure (median times, result)

  let (synthTime, synthGG) = case synthResult of
        Just (t, Right gg) -> (Just t, Just gg)
        Just (t, Left _)   -> (Just t, Nothing)
        Nothing            -> (Nothing, Nothing)

  -- Inferred global size (AST) and balancedness (untimed)
  let infSize = case synthGG of
        Just gg -> case globalGraphToType gg of
          Right gt -> Just (globalTypeSize gt)
          Left _   -> Nothing
        Nothing -> Nothing

      infBalanced = case synthGG of
        Just gg -> Just (checkBalanced gg == Right ())
        Nothing -> Nothing

  pure LocalBenchResult
    { lbrName             = pleName ex
    , lbrDisplayName      = pleDisplayName ex
    , lbrCitation         = pleCitation ex
    , lbrContextSize      = ctxSize
    , lbrNumParticipants  = nParticipants
    , lbrSafe             = safe
    , lbrDeadlockFree     = df
    , lbrLive             = live
    , lbrMpstkTime        = mpstkTime
    , lbrInferredSize     = infSize
    , lbrInferredBalanced = infBalanced
    , lbrSynthesisTime    = synthTime
    }

runSingleLocalAssociationCheck :: ParsedLocalExample -> IO AssociationCheckResult
runSingleLocalAssociationCheck ex = do
  let context = pleContext ex
      participantOrder = pleParticipantOrder ex
      originalGraphs =
        Map.fromList
          [ (p, buildLocalGraph lt)
          | p <- participantOrder
          , Just lt <- [Map.lookup p context]
          ]
      contextGraph = buildContextGraph (Map.toList originalGraphs)

  hPutStrLn stderr $ "  [Check] " ++ pleName ex

  case synthesise contextGraph of
    Left synthErr ->
      pure AssociationCheckResult
        { acrName = pleName ex
        , acrDisplayName = pleDisplayName ex
        , acrPassed = False
        , acrMessage = "synthesis failed: " ++ show synthErr
        , acrInferredSize = Nothing
        }
    Right gg -> do
      let inferredSize = either (const Nothing) (Just . globalTypeSize) (globalGraphToType gg)
      case mapM (\p -> fmap ((,) p) (projectCoinductiveFull gg p)) participantOrder of
        Left projErr ->
          pure AssociationCheckResult
            { acrName = pleName ex
            , acrDisplayName = pleDisplayName ex
            , acrPassed = False
            , acrMessage = "projection failed: " ++ show projErr
            , acrInferredSize = inferredSize
            }
        Right projectedEntries ->
          case checkContextSubtype originalGraphs (Map.fromList projectedEntries) of
            Right () ->
              pure AssociationCheckResult
                { acrName = pleName ex
                , acrDisplayName = pleDisplayName ex
                , acrPassed = True
                , acrMessage = "ok"
                , acrInferredSize = inferredSize
                }
            Left subtypingErrs ->
              pure AssociationCheckResult
                { acrName = pleName ex
                , acrDisplayName = pleDisplayName ex
                , acrPassed = False
                , acrMessage = "subtyping failed: " ++ show subtypingErrs
                , acrInferredSize = inferredSize
                }

-- ---------------------------------------------------------------------------
-- Timing helpers
-- ---------------------------------------------------------------------------

-- | Time an IO action using a monotonic clock. Performs GC before timing
-- to reduce noise, and forces the result to NF.
timeIO :: NFData a => IO a -> IO (NominalDiffTime, a)
timeIO action = do
  performGC
  start <- getMonotonicTimeNSec
  result <- action >>= evaluate . force
  end <- getMonotonicTimeNSec
  pure (nsToNDT (end - start), result)

-- | Time an IO action, discarding the result.
timeIO_ :: IO () -> IO NominalDiffTime
timeIO_ action = do
  performGC
  start <- getMonotonicTimeNSec
  _ <- action
  end <- getMonotonicTimeNSec
  pure (nsToNDT (end - start))

-- | Convert nanoseconds to NominalDiffTime.
nsToNDT :: Word64 -> NominalDiffTime
nsToNDT ns = realToFrac (fromIntegral ns / (1e9 :: Double))

median :: [NominalDiffTime] -> NominalDiffTime
median xs =
  let sorted = sort xs
   in sorted !! (length sorted `div` 2)

-- | Benchmark a single projection variant. Returns Nothing on timeout.
-- Each iteration: syntax→graph→project→graph→syntax (full round-trip).
-- Rebuilds per iteration to prevent GHC full-laziness sharing.
benchVariant timeoutUs runs gt participants projFn =
  timeout timeoutUs $ do
    times <- sequence
      [ timeIO_ $ do
          gg' <- evaluate $ force $ buildGlobalGraph gt
          lgs <- evaluate $ force [projFn gg' p | p <- participants]
          -- Convert projected graphs back to syntax
          _ <- evaluate $ force [localGraphToType lg | Right lg <- lgs]
          pure ()
      | _ <- [1..runs]
      ]
    let t = median times
        gg = buildGlobalGraph gt
        allOk = all (\p -> case projFn gg p of Right _ -> True; Left _ -> False) participants
    pure (ProjVariantResult t allOk)

-- | Run an action with the mpstk semaphore held, if one is provided.
withMpstkSem :: Maybe QSem -> IO a -> IO a
withMpstkSem Nothing    action = action
withMpstkSem (Just sem) action = bracket_ (waitQSem sem) (signalQSem sem) action
