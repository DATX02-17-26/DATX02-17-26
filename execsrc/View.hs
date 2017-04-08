module Main where

import System.Environment
import System.FilePath

import NormalizationStrategies
import CoreS.AST
import CoreS.Parse
import CoreS.ConvBack
import Norm.AllNormalizations as ALL

main :: IO ()
main = do
  args <- getArgs
  case args of
    [studPath] -> do
      studContents <- readFile studPath
      putStrLn "FILE CONTENTS BEFORE NORMALIZATION:\n"
      putStrLn studContents
      let Right ast = parseConv studContents
      putStrLn "\nAFTER NORMALIZATION:\n"
      putStrLn $ either (const "parse error") id $ prettyCore' (normalize ast)
    _ -> putStrLn "Bad args"

normalize :: CompilationUnit -> CompilationUnit
normalize = executeNormalizer ALL.normalizations
