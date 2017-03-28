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
  return $ (nf (uncurry (matches normalizeUAST)) (AST.toUnitype $ normalize stud, AST.toUnitype $ normalize mod))

main :: IO ()
main = do
  wide <- makeBench "bench/Programs/wideStudent.java" "bench/Programs/wideModel.java"
  same <- makeBench "bench/Programs/wideModel.java" "bench/Programs/wideModel.java"
  defaultMain
    [ bench "wide" wide
    , bench "same" same
    ]
