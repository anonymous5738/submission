{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Benchmark.Catalogue (globalExamples, localExamples, parseGlobalExample, parseLocalExample)
import Benchmark.LaTeX (renderGlobalTable, renderLocalTable, renderLaTeXDocument)
import Benchmark.Runner
  ( AssociationCheckResult(..)
  , GlobalBenchResult(..)
  , LocalBenchResult(..)
  , ProjVariantResult(..)
  , runGlobalBenchmarks
  , runLocalAssociationChecks
  , runLocalBenchmarks
  , runSingleGlobalBenchmark
  , runSingleLocalAssociationCheck
  , runSingleLocalBenchmark
  )
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem)
import Control.Exception (SomeException, catch)
import Data.Time.Clock (NominalDiffTime)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr)
import System.Process (readProcess)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case parseArgs args defaultConfig >>= validateConfig of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr ""
      hPutStrLn stderr usage
      exitFailure
    Right config
      | cfgShowHelp config -> putStrLn usage >> exitSuccess
      | cfgCheck config -> runCheckMode config
      | otherwise -> runBenchmarkMode config

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgShowHelp   :: Bool
  , cfgCheck      :: Bool
  , cfgLatex      :: Bool
  , cfgNoMpstk    :: Bool
  , cfgRuns       :: Int
  , cfgTimeout    :: Int
  , cfgOutput     :: Maybe FilePath
  , cfgStandalone :: Bool
  , cfgParallel   :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgShowHelp   = False
  , cfgCheck      = False
  , cfgLatex      = False
  , cfgNoMpstk    = False
  , cfgRuns       = 10
  , cfgTimeout    = 60
  , cfgOutput     = Nothing
  , cfgStandalone = False
  , cfgParallel   = False
  }

parseArgs :: [String] -> Config -> Either String Config
parseArgs [] cfg = Right cfg
parseArgs ("--help" : rest) cfg = parseArgs rest cfg { cfgShowHelp = True }
parseArgs ("-h" : rest) cfg = parseArgs rest cfg { cfgShowHelp = True }
parseArgs ("--check" : rest) cfg = parseArgs rest cfg { cfgCheck = True }
parseArgs ("--latex" : rest) cfg = parseArgs rest cfg { cfgLatex = True }
parseArgs ("--no-mpstk" : rest) cfg = parseArgs rest cfg { cfgNoMpstk = True }
parseArgs ("--standalone" : rest) cfg = parseArgs rest cfg { cfgLatex = True, cfgStandalone = True }
parseArgs ("--parallel" : rest) cfg = parseArgs rest cfg { cfgParallel = True }
parseArgs ("--runs" : n : rest) cfg = parsePositiveInt "runs" n >>= \m -> parseArgs rest cfg { cfgRuns = m }
parseArgs ("--timeout" : n : rest) cfg = parsePositiveInt "timeout" n >>= \m -> parseArgs rest cfg { cfgTimeout = m }
parseArgs ("-t" : n : rest) cfg = parsePositiveInt "timeout" n >>= \m -> parseArgs rest cfg { cfgTimeout = m }
parseArgs ("-o" : path : rest) cfg = parseArgs rest cfg { cfgOutput = Just path }
parseArgs ("--output" : path : rest) cfg = parseArgs rest cfg { cfgOutput = Just path }
parseArgs (arg : _) _ = Left ("Unknown argument: " ++ arg)

validateConfig :: Config -> Either String Config
validateConfig cfg
  | cfgCheck cfg && cfgLatex cfg =
      Left "Options --check and --latex cannot be used together."
  | otherwise =
      Right cfg

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt field value =
  case reads value of
    [(n, "")] | n > 0 -> Right n
    _ -> Left ("Invalid value for --" ++ field ++ ": " ++ value)

usage :: String
usage = unlines
  [ "Usage: mpst-bench [--check] [--latex] [--standalone] [--parallel]"
  , "                  [--runs N] [--timeout N] [--no-mpstk] [-o PATH]"
  , ""
  , "Modes:"
  , "  default     run the benchmark suite and print a text summary"
  , "  --latex     emit LaTeX tables instead of the text summary"
  , "  --check     synthesise each local benchmark, reproject it, and"
  , "              check that the original context is a subtype of the"
  , "              projected one"
  , ""
  , "Note: --check and --latex are separate modes."
  ]

detectMpstk :: IO Bool
detectMpstk = do
  (readProcess "mpstk" ["verify", "--help"] "" >> pure True)
    `catch` (\(_ :: SomeException) -> pure False)

runBenchmarkMode :: Config -> IO ()
runBenchmarkMode config = do
  mpstkAvailable <- if cfgNoMpstk config
    then pure False
    else detectMpstk

  hPutStrLn stderr "Configuration:"
  hPutStrLn stderr $ "  Runs per benchmark: " ++ show (cfgRuns config)
  hPutStrLn stderr $ "  mpstk available:    " ++ show mpstkAvailable
  hPutStrLn stderr $ "  Timeout (s):        " ++ show (cfgTimeout config)
  hPutStrLn stderr $ "  Parallel:           " ++ show (cfgParallel config)
  hPutStrLn stderr $ "  Output:             " ++ maybe "stdout" id (cfgOutput config)
  hPutStrLn stderr $ "  LaTeX:              " ++ show (cfgLatex config)
  hPutStrLn stderr $ "  Standalone:         " ++ show (cfgStandalone config)
  hPutStrLn stderr ""

  let globalParseResults = map parseGlobalExample globalExamples
  parsedGlobals <- case sequence globalParseResults of
    Left err -> do
      hPutStrLn stderr $ "Global parse error: " ++ err
      exitFailure
    Right gs -> do
      hPutStrLn stderr $ "Parsed " ++ show (length gs) ++ " global examples."
      pure gs

  let localParseResults = map parseLocalExample localExamples
  parsedLocals <- case sequence localParseResults of
    Left err -> do
      hPutStrLn stderr $ "Local parse error: " ++ err
      exitFailure
    Right ls -> do
      hPutStrLn stderr $ "Parsed " ++ show (length ls) ++ " local examples."
      pure ls

  hPutStrLn stderr ""

  mpstkSem <- if mpstkAvailable && cfgParallel config
    then Just <$> newQSem 4
    else pure Nothing

  let nRuns     = cfgRuns config
      timeoutS  = cfgTimeout config

  hPutStrLn stderr "Running global benchmarks..."
  globalResults <- if cfgParallel config
    then mapConcurrently (runSingleGlobalBenchmark nRuns timeoutS) parsedGlobals
    else runGlobalBenchmarks nRuns timeoutS parsedGlobals

  hPutStrLn stderr "Running local benchmarks..."
  localResults <- if cfgParallel config
    then mapConcurrently (runSingleLocalBenchmark mpstkAvailable nRuns timeoutS mpstkSem) parsedLocals
    else runLocalBenchmarks mpstkAvailable nRuns timeoutS mpstkSem parsedLocals

  let report
        | cfgLatex config =
            if cfgStandalone config
              then renderLaTeXDocument nRuns globalResults localResults
              else renderGlobalTable nRuns globalResults ++ "\n" ++ renderLocalTable nRuns localResults
        | otherwise =
            unlines
              [ "=== Global Results ==="
              , renderGlobalSummary globalResults
              , "=== Local Results ==="
              , renderLocalSummary localResults
              ]
  emitReport config report

runCheckMode :: Config -> IO ()
runCheckMode config = do
  hPutStrLn stderr "Configuration:"
  hPutStrLn stderr $ "  Parallel: " ++ show (cfgParallel config)
  hPutStrLn stderr $ "  Output:   " ++ maybe "stdout" id (cfgOutput config)
  hPutStrLn stderr ""

  let localParseResults = map parseLocalExample localExamples
  parsedLocals <- case sequence localParseResults of
    Left err -> do
      hPutStrLn stderr $ "Local parse error: " ++ err
      exitFailure
    Right ls -> do
      hPutStrLn stderr $ "Parsed " ++ show (length ls) ++ " local examples."
      pure ls

  hPutStrLn stderr ""
  hPutStrLn stderr "Checking synthesis association property..."
  checkResults <- if cfgParallel config
    then mapConcurrently runSingleLocalAssociationCheck parsedLocals
    else runLocalAssociationChecks parsedLocals

  let report = renderAssociationSummary checkResults
  emitReport config report

  if all acrPassed checkResults
    then exitSuccess
    else exitFailure

emitReport :: Config -> String -> IO ()
emitReport config report =
  case cfgOutput config of
    Nothing   -> putStr report
    Just path -> writeFile path report >> hPutStrLn stderr ("Report written to " ++ path)

-- ---------------------------------------------------------------------------
-- Human-readable summaries
-- ---------------------------------------------------------------------------

renderGlobalSummary :: [GlobalBenchResult] -> String
renderGlobalSummary results =
  let header = padR 30 "Example"
            ++ padR 6 "|G|"
            ++ padR 6 "|pt|"
            ++ padR 6 "Bal"
            ++ padR 10 "BalTime"
            ++ padR 8 "|D_p|"
            ++ padR 10 "IP"
            ++ padR 10 "IF"
            ++ padR 10 "CP"
            ++ padR 10 "CF"
      sep = replicate (length header) '-'
      rows = map globalSummaryRow results
   in unlines (header : sep : rows)

globalSummaryRow :: GlobalBenchResult -> String
globalSummaryRow br =
  padR 30 (gbrDisplayName br)
    ++ padR 6 (show (gbrGlobalSize br))
    ++ padR 6 (show (gbrNumParticipants br))
    ++ padR 6 (if gbrBalanced br then "yes" else "no")
    ++ padR 10 (formatTimeHuman (gbrBalancedTime br))
    ++ padR 8 (maybe "---" show (gbrProjectedSize br))
    ++ padR 10 (fmtVariantH (gbrIP br))
    ++ padR 10 (fmtVariantH (gbrIF br))
    ++ padR 10 (fmtVariantH (gbrCP br))
    ++ padR 10 (fmtVariantH (gbrCF br))

renderLocalSummary :: [LocalBenchResult] -> String
renderLocalSummary results =
  let header = padR 30 "Example"
            ++ padR 6 "|D|"
            ++ padR 6 "|dom|"
            ++ padR 6 "safe"
            ++ padR 6 "df"
            ++ padR 6 "live"
            ++ padR 10 "mpstk"
            ++ padR 8 "|G_i|"
            ++ padR 6 "Bal"
            ++ padR 10 "Synth."
      sep = replicate (length header) '-'
      rows = map localSummaryRow results
   in unlines (header : sep : rows)

localSummaryRow :: LocalBenchResult -> String
localSummaryRow br =
  padR 30 (lbrDisplayName br)
    ++ padR 6 (show (lbrContextSize br))
    ++ padR 6 (show (lbrNumParticipants br))
    ++ padR 6 (fmtBoolH (lbrSafe br))
    ++ padR 6 (fmtBoolH (lbrDeadlockFree br))
    ++ padR 6 (fmtBoolH (lbrLive br))
    ++ padR 10 (maybe "---" formatTimeHuman (lbrMpstkTime br))
    ++ padR 8 (maybe "---" show (lbrInferredSize br))
    ++ padR 6 (fmtBoolH (lbrInferredBalanced br))
    ++ padR 10 (maybe "---" formatTimeHuman (lbrSynthesisTime br))

renderAssociationSummary :: [AssociationCheckResult] -> String
renderAssociationSummary results =
  let header = padR 30 "Example"
            ++ padR 8 "Check"
            ++ padR 8 "|G_i|"
            ++ "Details"
      sep = replicate (length header) '-'
      rows = map associationSummaryRow results
      passed = length (filter acrPassed results)
      total = length results
   in unlines
        ( [header, sep]
        ++ rows
        ++ [ ""
           , "Passed " ++ show passed ++ " / " ++ show total ++ " checks."
           ]
        )

associationSummaryRow :: AssociationCheckResult -> String
associationSummaryRow acr =
  padR 30 (acrDisplayName acr)
    ++ padR 8 (if acrPassed acr then "ok" else "fail")
    ++ padR 8 (maybe "---" show (acrInferredSize acr))
    ++ acrMessage acr

fmtVariantH :: Maybe ProjVariantResult -> String
fmtVariantH Nothing = "---"
fmtVariantH (Just pv)
  | pvAllOk pv = formatTimeHuman (pvTime pv)
  | otherwise   = "fail"

fmtBoolH :: Maybe Bool -> String
fmtBoolH Nothing      = "---"
fmtBoolH (Just True)  = "yes"
fmtBoolH (Just False) = "no"

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
