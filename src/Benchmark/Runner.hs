module Benchmark.Runner
  ( BenchResult(..)
  , ExampleMetrics(..)
  , TopDownResult(..)
  , BottomUpResult(..)
  , RoundTripResult(..)
  , runBenchmarks
  , runSingleBenchmark
  ) where

import Automata
  ( buildContextGraph
  , buildGlobalGraph
  , buildLocalGraph
  )
import Balanced (checkBalanced)
import Benchmark.Catalogue (Citation, ParsedExample(..))
import Benchmark.Size (globalTypeSize, contextSize)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import MpstkBackend (MpstkResults, mpstkVerify)
import Project (projectCoinductiveFull)
import Subtyping (checkContextSubtype)
import Synthesise (synthesise)

data ExampleMetrics = ExampleMetrics
  { emName            :: String
  , emCitation        :: Maybe Citation
  , emNumParticipants :: Int
  , emGlobalTypeSize  :: Maybe Int
  , emContextSize     :: Int
  }

data TopDownResult = TopDownResult
  { tdTime :: NominalDiffTime
  }

data BottomUpResult = BottomUpResult
  { buTime    :: NominalDiffTime
  , buResults :: MpstkResults
  }

data RoundTripResult = RoundTripResult
  { rtTime        :: NominalDiffTime
  , rtSynthesisOk :: Bool
  , rtBalancedOk  :: Maybe Bool
  , rtSubtypingOk :: Maybe Bool
  }

data BenchResult = BenchResult
  { brMetrics   :: ExampleMetrics
  , brTopDown   :: Maybe TopDownResult
  , brBottomUp  :: Maybe BottomUpResult
  , brRoundTrip :: RoundTripResult
  }

-- | Run all benchmarks with the given configuration.
runBenchmarks ::
  Bool ->   -- ^ whether to run mpstk (bottom-up)
  Int ->    -- ^ number of iterations per benchmark
  [ParsedExample] ->
  IO [BenchResult]
runBenchmarks useMpstk runs = mapM (runSingleBenchmark useMpstk runs)

-- | Run a single benchmark example.
runSingleBenchmark ::
  Bool ->
  Int ->
  ParsedExample ->
  IO BenchResult
runSingleBenchmark useMpstk runs ex = do
  let metrics = ExampleMetrics
        { emName            = peName ex
        , emCitation        = peCitation ex
        , emNumParticipants = Map.size (peContext ex)
        , emGlobalTypeSize  = globalTypeSize <$> peGlobalType ex
        , emContextSize     = contextSize (peContext ex)
        }

  topDown <- case peGlobalType ex of
    Nothing -> pure Nothing
    Just gt -> do
      t <- medianTimeIO runs $ do
        let gg = buildGlobalGraph gt
            participants = peParticipantOrder ex
        _ <- evaluate $ force
          [ projectCoinductiveFull gg p
          | p <- participants
          ]
        pure ()
      pure (Just (TopDownResult t))

  bottomUp <- if useMpstk
    then do
      (t, res) <- medianTimeIOWithResult runs $
        mpstkVerify (peContext ex)
      pure (Just (BottomUpResult t res))
    else pure Nothing

  roundTrip <- do
    t <- medianTimeIO runs $ do
      let localGraphs =
            [ (p, buildLocalGraph lt)
            | (p, lt) <- Map.toList (peContext ex)
            ]
          orderedLocalGraphs =
            [ (p, buildLocalGraph lt)
            | p <- peParticipantOrder ex
            , Just lt <- [Map.lookup p (peContext ex)]
            ]
          cg = buildContextGraph orderedLocalGraphs
      _ <- evaluate $ force $ case synthesise cg of
        Left err -> Left err
        Right gg ->
          let bal = checkBalanced gg
              projected = Map.fromList
                [ (p, lg)
                | (p, _) <- localGraphs
                , Right lg <- [projectCoinductiveFull gg p]
                ]
              origGraphs = Map.fromList localGraphs
              sub = checkContextSubtype origGraphs projected
           in Right (gg, bal, sub)
      pure ()
    -- Run once more to get the actual results (not timed)
    let localGraphs =
          [ (p, buildLocalGraph lt)
          | (p, lt) <- Map.toList (peContext ex)
          ]
        orderedLocalGraphs =
          [ (p, buildLocalGraph lt)
          | p <- peParticipantOrder ex
          , Just lt <- [Map.lookup p (peContext ex)]
          ]
        cg = buildContextGraph orderedLocalGraphs
        (synthOk, balOk, subOk) = case synthesise cg of
          Left _   -> (False, Nothing, Nothing)
          Right gg ->
            let bal = checkBalanced gg == Right ()
                projected = Map.fromList
                  [ (p, lg)
                  | (p, _) <- localGraphs
                  , Right lg <- [projectCoinductiveFull gg p]
                  ]
                origGraphs = Map.fromList localGraphs
                sub = checkContextSubtype origGraphs projected == Right ()
             in (True, Just bal, Just sub)
    pure (RoundTripResult t synthOk balOk subOk)

  pure BenchResult
    { brMetrics   = metrics
    , brTopDown   = topDown
    , brBottomUp  = bottomUp
    , brRoundTrip = roundTrip
    }

-- | Time an IO action and return the median time over N runs.
medianTimeIO :: Int -> IO () -> IO NominalDiffTime
medianTimeIO n action = do
  times <- sequence [timeIO_ action | _ <- [1..n]]
  pure (median times)

-- | Time an IO action, return median time and last result.
medianTimeIOWithResult :: Int -> IO a -> IO (NominalDiffTime, a)
medianTimeIOWithResult n action = do
  results <- sequence [timeIO action | _ <- [1..n]]
  let times = map fst results
      lastResult = snd (last results)
  pure (median times, lastResult)

timeIO :: IO a -> IO (NominalDiffTime, a)
timeIO action = do
  start <- getCurrentTime
  result <- action >>= evaluate
  end <- getCurrentTime
  pure (diffUTCTime end start, result)

timeIO_ :: IO () -> IO NominalDiffTime
timeIO_ action = fst <$> timeIO action

median :: [NominalDiffTime] -> NominalDiffTime
median xs =
  let sorted = sort xs
   in sorted !! (length sorted `div` 2)
