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

makeBench :: FilePath -> FilePath -> IO Benchmarkable
makeBench stud mod = do
  let paths = Ctx stud [mod]
  (Ctx (Right stud) [Right mod]) <- resultEvalM ((fmap parseConv) <$> readRawContents paths)
  return $ (nf (uncurry (matches normalizeUAST)) (AST.toUnitype $ normalize stud, AST.toUnitype $ mod))

main :: IO ()
main = do
  wide <- makeBench "bench/Programs/wideStudent.java" "bench/Programs/wideModel.java"
  defaultMain
    [ bench "matches wide" wide
    ]
