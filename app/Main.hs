module Main where

import MPST
import System.Environment (getArgs)
import Visualise

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse-global", src] ->
      case parseGlobalTypeChecked src of
        Left e -> putStrLn ("Invalid global type:\n" ++ e)
        Right g -> putStrLn (renderGlobalType g)
    ["parse-local", src] ->
      case parseLocalTypeChecked src of
        Left e -> putStrLn ("Invalid local type:\n" ++ e)
        Right l -> putStrLn (renderLocalType l)
    ["render-global", src, out] ->
      case parseGlobalTypeChecked src of
        Left e -> putStrLn ("Invalid global type:\n" ++ e)
        Right g -> do
          let graph = buildGlobalGraph g
          _ <- renderGlobalPng out graph
          putStrLn ("Wrote " ++ out)
    ["render-local", src, out] ->
      case parseLocalTypeChecked src of
        Left e -> putStrLn ("Invalid local type:\n" ++ e)
        Right l -> do
          let graph = buildLocalGraph l
          _ <- renderLocalPng out graph
          putStrLn ("Wrote " ++ out)
    _ -> putStrLn usage

usage :: String
usage =
  unlines
    [ "mpst usage:"
    , "  parse-global \"<global-type>\"         Parse and pretty-print a global type"
    , "  parse-local \"<local-type>\"           Parse and pretty-print a local type"
    , "  render-global \"<global-type>\" OUT.png  Parse, build automaton, render PNG"
    , "  render-local \"<local-type>\" OUT.png    Parse, build automaton, render PNG"
    ]
