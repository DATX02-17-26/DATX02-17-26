module Main where
import Criterion.Main

import GenStrat
import CoreS.Parse
import CoreS.AST
import qualified CoreS.ASTUnitype as AST
import AlphaR
import NormalizationStrategies hiding ((<>))
import EvaluationMonad
import SolutionContext

makeBench_fromUnitype :: FilePath -> IO Benchmarkable
makeBench_fromUnitype stud = do
  let paths = Ctx stud []
  (Ctx (Right stud) []) <- resultEvalM ((fmap parseConv) <$> readRawContents paths)
  return $ nf (AST.fromUnitype :: AST.AST -> CompilationUnit) $ (AST.toUnitype :: CompilationUnit -> AST.AST) stud

makeBench_toUnitype :: FilePath -> IO Benchmarkable
makeBench_toUnitype stud = do
  let paths = Ctx stud []
  (Ctx (Right stud) []) <- resultEvalM ((fmap parseConv) <$> readRawContents paths)
  return $ nf (AST.toUnitype :: CompilationUnit -> AST.AST) stud

main :: IO ()
main = do
  wide_from <- makeBench_fromUnitype "bench/Programs/wideStudent.java"
  wide_to <- makeBench_toUnitype "bench/Programs/wideStudent.java"
  defaultMain
    [ bench "wide_from" wide_from
    , bench "wide_to"   wide_to
    ]
