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

normalizations :: Normalizer CompilationUnit
normalizations = [ alphaRenaming ]

normalize :: CompilationUnit -> CompilationUnit
normalize = executeNormalizer normalizations

normalizeUAST :: AST.AST -> AST.AST
normalizeUAST  = AST.inCore normalize

makeBench :: FilePath -> IO Benchmarkable
makeBench stud = do
  let paths = Ctx stud []
  (Ctx (Right stud) []) <- resultEvalM ((fmap parseConv) <$> readRawContents paths)
  return $ nf normalize stud

main :: IO ()
main = do
  wide <- makeBench "bench/Programs/wideStudent.java"
  defaultMain
    [ bench "wide" wide
    ]
