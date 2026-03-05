{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Benchmark.Catalogue (benchmarkExamples, parseBenchExample)
import Benchmark.LaTeX (renderLaTeXDocument, renderLaTeXTable)
import Benchmark.Runner (BenchResult(..), ExampleMetrics(..), RoundTripResult(..), TopDownResult(..), BottomUpResult(..), runBenchmarks)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (SomeException, catch)
import Data.Time.Clock (NominalDiffTime)
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args defaultConfig

  -- Auto-detect mpstk availability
  mpstkAvailable <- if cfgNoMpstk config
    then pure False
    else detectMpstk

  hPutStrLn stderr $ "Configuration:"
  hPutStrLn stderr $ "  Runs per benchmark: " ++ show (cfgRuns config)
  hPutStrLn stderr $ "  mpstk available:    " ++ show mpstkAvailable
  hPutStrLn stderr $ "  Output:             " ++ maybe "stdout" id (cfgOutput config)
  hPutStrLn stderr $ "  Standalone:         " ++ show (cfgStandalone config)
  hPutStrLn stderr ""

  -- Parse all benchmark examples
  let parseResults = map parseBenchExample benchmarkExamples
  case sequence parseResults of
    Left err -> do
      hPutStrLn stderr $ "Parse error: " ++ err
      exitFailure
    Right examples -> do
      hPutStrLn stderr $ "Parsed " ++ show (length examples) ++ " benchmark examples."
      hPutStrLn stderr ""

      -- Run benchmarks
      results <- runBenchmarks mpstkAvailable (cfgRuns config) examples

      -- Print human-readable summary to stderr
      hPutStrLn stderr "Results:"
      hPutStrLn stderr (renderSummary results)

      -- Output LaTeX
      let latex = if cfgStandalone config
            then renderLaTeXDocument results
            else renderLaTeXTable results
      case cfgOutput config of
        Nothing   -> putStr latex
        Just path -> writeFile path latex >> hPutStrLn stderr ("LaTeX written to " ++ path)

data Config = Config
  { cfgNoMpstk    :: Bool
  , cfgRuns       :: Int
  , cfgOutput     :: Maybe FilePath
  , cfgStandalone :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgNoMpstk    = False
  , cfgRuns       = 10
  , cfgOutput     = Nothing
  , cfgStandalone = False
  }

parseArgs :: [String] -> Config -> Config
parseArgs [] cfg = cfg
parseArgs ("--no-mpstk" : rest) cfg = parseArgs rest cfg { cfgNoMpstk = True }
parseArgs ("--standalone" : rest) cfg = parseArgs rest cfg { cfgStandalone = True }
parseArgs ("--runs" : n : rest) cfg = parseArgs rest cfg { cfgRuns = read n }
parseArgs ("-o" : path : rest) cfg = parseArgs rest cfg { cfgOutput = Just path }
parseArgs ("--output" : path : rest) cfg = parseArgs rest cfg { cfgOutput = Just path }
parseArgs (_ : rest) cfg = parseArgs rest cfg

detectMpstk :: IO Bool
detectMpstk = do
  (readProcess "mpstk" ["verify", "--help"] "" >> pure True)
    `catch` (\(_ :: SomeException) -> pure False)

renderSummary :: [BenchResult] -> String
renderSummary results =
  let header = padR 15 "Example" ++ padR 12 "Top-Down" ++ padR 12 "mpstk" ++ padR 12 "Round-Trip" ++ "Synth/Bal/Sub"
      sep    = replicate (length header) '-'
      rows   = map summaryRow results
   in unlines (header : sep : rows)

summaryRow :: BenchResult -> String
summaryRow br =
  let m = brMetrics br
      td = maybe "---" (formatTimeHuman . tdTime) (brTopDown br)
      bu = maybe "---" (formatTimeHuman . buTime) (brBottomUp br)
      rt = formatTimeHuman (rtTime (brRoundTrip br))
      rr = brRoundTrip br
      status = intercalate "/"
        [ if rtSynthesisOk rr then "ok" else "FAIL"
        , maybe "---" (\b -> if b then "ok" else "FAIL") (rtBalancedOk rr)
        , maybe "---" (\b -> if b then "ok" else "FAIL") (rtSubtypingOk rr)
        ]
   in padR 15 (emName m) ++ padR 12 td ++ padR 12 bu ++ padR 12 rt ++ status

padR :: Int -> String -> String
padR n s
  | length s >= n = s
  | otherwise     = s ++ replicate (n - length s) ' '

formatTimeHuman :: NominalDiffTime -> String
formatTimeHuman dt
  | us < 1000    = showN 1 us ++ " us"
  | us < 1000000 = showN 1 (us / 1000) ++ " ms"
  | otherwise     = showN 2 (us / 1000000) ++ " s"
  where
    us = realToFrac dt * 1000000 :: Double

showN :: Int -> Double -> String
showN n x =
  let factor = 10 ^ n :: Int
      rounded = fromIntegral (round (x * fromIntegral factor) :: Int) / fromIntegral factor :: Double
      intPart = truncate rounded :: Int
      fracPart = abs (round ((rounded - fromIntegral intPart) * fromIntegral (10 ^ n :: Int)) :: Int)
      fracStr = let s = show fracPart in replicate (n - length s) '0' ++ s
   in show intPart ++ "." ++ fracStr
