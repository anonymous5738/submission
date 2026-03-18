module Benchmark.LaTeX
  ( renderGlobalTable
  , renderLocalTable
  , renderLaTeXDocument
  ) where

import Benchmark.Types (Citation(..))
import Benchmark.Runner
  ( GlobalBenchResult(..)
  , LocalBenchResult(..)
  , ProjVariantResult(..)
  )
import Data.List (intercalate, isPrefixOf)
import Data.Time.Clock (NominalDiffTime)

-- ---------------------------------------------------------------------------
-- Global table
-- ---------------------------------------------------------------------------

-- | Render the global benchmarks table.
renderGlobalTable :: Int -> [GlobalBenchResult] -> String
renderGlobalTable numRuns results = unlines
  [ "\\begin{table}[t]"
  , "\\centering"
  , "\\caption{Top-down projection benchmarks: median times over $" ++ show numRuns ++ "$ runs."
  , "  Here $|G|$ is the size of the global type, \\#p is its"
  , "  number of participants, Balanced reports whether $G$ is"
  , "  balanced ($\\checkmark$) or not ($\\times$), with the accompanying"
  , "  time giving the cost of the balancedness check, and"
  , "  $|\\Delta_{\\textsf{pro}}|$ is the size of the projected local context."
  , "  The Citation column gives the source citation and, where available,"
  , "  the corresponding entry in Appendix Table~\\ref{tab:example-global-types}."
  , "  IP = inductive plain, IF = inductive full, CP = coinductive plain,"
  , "  and CF = coinductive full; each projection column shows the runtime"
  , "  when projection succeeds and $\\times$ when that algorithm fails.}"
  , "\\label{tab:global-benchmarks}"
  , "\\resizebox{\\textwidth}{!}{"
  , "\\scriptsize"
  , "\\begin{tabular}{l l r r l r r r r r}"
  , "\\toprule"
  , "Example & Citation & $|G|$ & \\#p"
    ++ " & Balanced & $|\\Delta_{\\textsf{pro}}|$"
    ++ " & IP & IF & CP & CF \\\\"
  , "\\midrule"
  , intercalate "\n" (map renderGlobalRow results)
  , "\\bottomrule"
  , "\\end{tabular}"
  , "}"
  , "\\end{table}"
  ]

renderGlobalRow :: GlobalBenchResult -> String
renderGlobalRow br =
  let name = gbrDisplayName br
      cite = formatCitationCell (gbrCitation br) (appendixRefForName (gbrName br))
      gSize = show (gbrGlobalSize br)
      nPart = show (gbrNumParticipants br)
      bal = (if gbrBalanced br then "{\\color{green!50!black}$\\checkmark$}" else "{\\color{red!70!black}$\\times$}")
            ++ " " ++ formatTime (gbrBalancedTime br)
      projSize = maybe "---" show (gbrProjectedSize br)
      ip  = fmtVariant (gbrIP br)
      ifu = fmtVariant (gbrIF br)
      cp  = fmtVariant (gbrCP br)
      cf  = fmtVariant (gbrCF br)
   in intercalate " & " [name, cite, gSize, nPart, bal, projSize, ip, ifu, cp, cf]
        ++ " \\\\"

-- ---------------------------------------------------------------------------
-- Local table
-- ---------------------------------------------------------------------------

-- | Render the local benchmarks table.
renderLocalTable :: Int -> [LocalBenchResult] -> String
renderLocalTable numRuns results = unlines
  [ "\\begin{table}[t]"
  , "\\centering"
  , "\\caption{Bottom-up synthesis benchmarks: median times over $" ++ show numRuns ++ "$ runs."
  , "  Here $|\\Delta|$ is the size of the input local context,"
  , "  \\#p is its number of participants,"
  , "  $\\textsf{safe}$, $\\textsf{df}$, and $\\textsf{live}$ report whether"
  , "  the context is safe, deadlock-free, and live respectively"
  , "  ($\\checkmark$/$\\times$),"
  , "  {\\sf mpstk} is the time taken to check these properties,"
  , "  $|G_{\\textsf{inf}}|$ is the size of the synthesised global type,"
  , "  Balanced reports whether the synthesised"
  , "  global type is balanced ($\\checkmark$/$\\times$), and"
  , "  Synth.\\ is the synthesis time."
  , "  The Citation column gives the source citation and, where available,"
  , "  the corresponding appendix reference, including"
  , "  Table~\\ref{tab:example-global-types} and"
  , "  Example~\\ref{ex:binary-counter}."
  , "  Entries marked ``---'' indicate cases where the tool exceeded available"
  , "  resources.}"
  , "\\label{tab:local-benchmarks}"
  , "\\resizebox{\\textwidth}{!}{"
  , "\\scriptsize"
  , "\\begin{tabular}{l l r r c c c r r c r}"
  , "\\toprule"
  , "Example & Citation & $|\\Delta|$ & \\#p"
    ++ " & $\\textsf{safe}$ & $\\textsf{df}$ & $\\textsf{live}$"
    ++ " & mpstk & $|G_{\\textsf{inf}}|$ & Balanced & Synth. \\\\"
  , "\\midrule"
  , intercalate "\n" (map renderLocalRow results)
  , "\\bottomrule"
  , "\\end{tabular}"
  , "}"
  , "\\end{table}"
  ]

renderLocalRow :: LocalBenchResult -> String
renderLocalRow br =
  let name = lbrDisplayName br
      cite = formatCitationCell (lbrCitation br) (appendixRefForName (lbrName br))
      ctxSize = show (lbrContextSize br)
      nPart = show (lbrNumParticipants br)
      safe = fmtBool (lbrSafe br)
      df   = fmtBool (lbrDeadlockFree br)
      live = fmtBool (lbrLive br)
      mpstk = maybe "---" formatTime (lbrMpstkTime br)
      infSize = maybe "---" show (lbrInferredSize br)
      infBal = fmtBool (lbrInferredBalanced br)
      synth = maybe "---" formatTime (lbrSynthesisTime br)
   in intercalate " & " [name, cite, ctxSize, nPart, safe, df, live, mpstk, infSize, infBal, synth]
        ++ " \\\\"

-- ---------------------------------------------------------------------------
-- Standalone document
-- ---------------------------------------------------------------------------

-- | Render a standalone compilable LaTeX document with both tables.
renderLaTeXDocument :: Int -> [GlobalBenchResult] -> [LocalBenchResult] -> String
renderLaTeXDocument numRuns globalResults localResults = unlines
  [ "\\documentclass{article}"
  , "\\usepackage{booktabs}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}"
  , "\\usepackage{graphicx}"
  , "\\usepackage{xcolor}"
  , "\\providecommand{\\exampletabref}[1]{Appendix example (#1)}"
  , "\\begin{document}"
  , ""
  , renderGlobalTable numRuns globalResults
  , ""
  , renderLocalTable numRuns localResults
  , ""
  , "\\end{document}"
  ]

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

-- | Format a source citation with optional example reference.
formatSourceCite :: Maybe Citation -> Maybe String
formatSourceCite Nothing = Nothing
formatSourceCite (Just c) = Just $ case citeRef c of
  Nothing  -> "\\cite{" ++ citeKey c ++ "}"
  Just ref -> "\\cite[" ++ ref ++ "]{" ++ citeKey c ++ "}"

formatCitationCell :: Maybe Citation -> Maybe String -> String
formatCitationCell cite appRef =
  case (formatSourceCite cite, appRef) of
    (Nothing, Nothing) -> "---"
    (Just c, Nothing)  -> c
    (Nothing, Just r)  -> r
    (Just c, Just r)   -> c ++ "; " ++ r

appendixRefForName :: String -> Maybe String
appendixRefForName name
  | name == "G_oe"              = Just "\\exampletabref{a}"
  | name == "G_oa"              = Just "\\exampletabref{b}"
  | name == "G_tb"              = Just "\\exampletabref{c}"
  | "G_mr-" `isPrefixOf` name   = Just "\\exampletabref{d}"
  | "G_iw-" `isPrefixOf` name   = Just "\\exampletabref{e}"
  | name == "G_itp"             = Just "\\exampletabref{f}"
  | name == "G_sta"             = Just "\\exampletabref{g}"
  | name == "G_bta"             = Just "\\exampletabref{h}"
  | name == "G_ip"              = Just "\\exampletabref{i}"
  | name == "G_if"              = Just "\\exampletabref{j}"
  | name == "G_ring"            = Just "\\exampletabref{k}"
  | name == "G_adder"           = Just "\\exampletabref{l}"
  | "BinCtr-" `isPrefixOf` name = Just "Example~\\ref{ex:binary-counter}"
  | otherwise                   = Nothing

-- | Format a projection variant: time if ok, ✗ if failed, --- if timed out.
fmtVariant :: Maybe ProjVariantResult -> String
fmtVariant Nothing = "---"
fmtVariant (Just pv)
  | pvAllOk pv = formatTime (pvTime pv)
  | otherwise   = "{\\color{red!70!black}$\\times$}"

-- | Format a boolean result.
fmtBool :: Maybe Bool -> String
fmtBool Nothing      = "---"
fmtBool (Just True)  = "{\\color{green!50!black}$\\checkmark$}"
fmtBool (Just False) = "{\\color{red!70!black}$\\times$}"

-- | Format a time duration adaptively.
-- Microsecond results are uncoloured; millisecond and second results are
-- coloured to stand out.
formatTime :: NominalDiffTime -> String
formatTime dt
  | us < 1000    = showFixed 1 us ++ "\\,\\textmu s"
  | us < 1000000 = "{\\color{blue!70!black}" ++ showFixed 1 (us / 1000) ++ "\\,ms}"
  | otherwise     = "{\\color{blue!70!black}" ++ showFixed 2 (us / 1000000) ++ "\\,s}"
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
