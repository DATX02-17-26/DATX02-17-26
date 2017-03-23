module TestStrategies where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad

import EvaluationMonad
import GenStrat
import SolutionContext
import AlphaR
import NormalizationStrategies
import CoreS.AST
import qualified CoreS.ASTUnitype as AST
import CoreS.Parse

normalizations :: Normalizer CompilationUnit
normalizations = [ alphaRenaming ]

normalize :: CompilationUnit -> CompilationUnit
normalize = executeNormalizer normalizations

normalizeUAST :: AST.AST -> AST.AST
normalizeUAST  = AST.inCore normalize

checkMatches :: FilePath -> FilePath -> IO Bool
checkMatches stud mod = do
  let paths = Ctx stud [mod]
  (Ctx (Right stud) [Right mod]) <- resultEvalM ((fmap parseConv) <$> readRawContents paths)
  return $ matches normalizeUAST (AST.toUnitype $ normalize stud) (AST.toUnitype (normalize mod))

{- Tests -}
test0 :: IO Bool
test0 = checkMatches "Test/Student_solutions/helloWorld0.java" "Test/Model_solutions/helloWorld0.java"

allTests :: TestTree 
allTests = testGroup "Strategies tests"
  [ testCase "helloWorld" $ assert test0
  ]
