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

matchesItself :: FilePath -> IO Bool
matchesItself x = checkMatches x x

{- Tests -}
test0 :: IO Bool
test0 = checkMatches "Test/fixture/strategies/helloWorld_student.java" "Test/fixture/strategies/helloWorld_model.java"

test1 :: IO Bool
test1 = matchesItself "Test/fixture/strategies/helloWorld_student.java"

allTests :: TestTree 
allTests = testGroup "Strategies tests"
  [ testCase "helloWorld"    $ assert test0
  , testCase "matchesItself" $ assert test1
  ]
