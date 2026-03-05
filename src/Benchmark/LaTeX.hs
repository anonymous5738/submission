module Benchmark.LaTeX
  ( renderLaTeXTable
  , renderLaTeXDocument
  ) where

import Benchmark.Catalogue (Citation(..))
import Benchmark.Runner
  ( BenchResult(..)
  , BottomUpResult(..)
  , ExampleMetrics(..)
  , RoundTripResult(..)
  , TopDownResult(..)
  )
import Data.List (intercalate)
import Data.Time.Clock (NominalDiffTime)

-- | Render just the LaTeX table environment.
renderLaTeXTable :: [BenchResult] -> String
renderLaTeXTable results = unlines
  [ "\\begin{table}[t]"
  , "\\centering"
  , "\\caption{Benchmark results: median times over $N$ runs.}"
  , "\\label{tab:benchmarks}"
  , "\\begin{tabular}{l l r r r r r r}"
  , "\\toprule"
  , "Example & Citation & $|\\mathcal{P}|$ & $|G|$ & $|\\Delta|$ & Top-Down & mpstk & Round-Trip \\\\"
  , "\\midrule"
  , intercalate "\n" (map renderRow results)
  , "\\bottomrule"
  , "\\end{tabular}"
  , "\\end{table}"
  ]

-- | Render a standalone compilable LaTeX document.
renderLaTeXDocument :: [BenchResult] -> String
renderLaTeXDocument results = unlines
  [ "\\documentclass{article}"
  , "\\usepackage{booktabs}"
  , "\\usepackage{amsmath}"
  , "\\begin{document}"
  , ""
  , renderLaTeXTable results
  , ""
  , "\\end{document}"
  ]

renderRow :: BenchResult -> String
renderRow br =
  let m = brMetrics br
      name = escapeLaTeX (emName m)
      cite = case emCitation m of
        Nothing -> "---"
        Just c  -> "\\cite{" ++ citeKey c ++ "}"
      nParticipants = show (emNumParticipants m)
      gSize = maybe "---" show (emGlobalTypeSize m)
      dSize = show (emContextSize m)
      topDown = case brTopDown br of
        Nothing -> "---"
        Just td -> formatTime (tdTime td)
      bottomUp = case brBottomUp br of
        Nothing -> "---"
        Just bu -> formatTime (buTime bu)
      roundTrip = formatTime (rtTime (brRoundTrip br))
   in name ++ " & " ++ cite ++ " & " ++ nParticipants ++ " & "
        ++ gSize ++ " & " ++ dSize ++ " & "
        ++ topDown ++ " & " ++ bottomUp ++ " & " ++ roundTrip ++ " \\\\"

-- | Format a time duration adaptively.
formatTime :: NominalDiffTime -> String
formatTime dt
  | us < 1000    = showFixed 1 us ++ "\\,\\textmu s"
  | us < 1000000 = showFixed 1 (us / 1000) ++ "\\,ms"
  | otherwise     = showFixed 2 (us / 1000000) ++ "\\,s"
  where
    us = realToFrac dt * 1000000 :: Double

-- | Show a Double with a fixed number of decimal places.
showFixed :: Int -> Double -> String
showFixed n x =
  let factor = 10 ^ n :: Int
      rounded = fromIntegral (round (x * fromIntegral factor) :: Int) / fromIntegral factor :: Double
   in showFFloat' n rounded

showFFloat' :: Int -> Double -> String
showFFloat' n x =
  let intPart = truncate x :: Int
      fracPart = abs (round ((x - fromIntegral intPart) * fromIntegral (10 ^ n :: Int)) :: Int)
      fracStr = padLeft n '0' (show fracPart)
   in show intPart ++ "." ++ fracStr

padLeft :: Int -> Char -> String -> String
padLeft n c s
  | length s >= n = s
  | otherwise     = replicate (n - length s) c ++ s

-- | Escape special LaTeX characters.
escapeLaTeX :: String -> String
escapeLaTeX = concatMap escapeChar
  where
    escapeChar '_' = "\\_"
    escapeChar '&' = "\\&"
    escapeChar '%' = "\\%"
    escapeChar '#' = "\\#"
    escapeChar c   = [c]
