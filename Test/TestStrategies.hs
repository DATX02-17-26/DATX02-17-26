module TestStrategies where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad

import EvaluationMonad
import GenStrat
import SolutionContext
import AlphaR
import NormalizationStrategies
import CoreS.AST
import qualified CoreS.ASTUnitype as AST
import CoreS.Parse
import Data.RoseTree
import Data.List

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

leftmost :: RoseTree a -> a
leftmost (RoseTree a []) = a
leftmost (RoseTree _ (t:_)) = leftmost t

selfIsLeftmost :: FilePath -> IO Bool
selfIsLeftmost sol = do
  let paths = Ctx sol []
  (Ctx (Right sol) []) <- resultEvalM ((fmap parseConv) <$> readRawContents paths)
  return $ leftmost (makeASTsRoseTree (makeStrategy (AST.toUnitype $ normalize sol))) == (AST.toUnitype $ normalize sol)

prop_dagHelper_unchanging :: [Int] -> Property
prop_dagHelper_unchanging as = (not $ elem 0 as) ==>
  null (deleteFirstsBy (==) ts after)
  where
    after = map fst $ dagHelper (\x y -> 0 == x `mod` y) ts []
    ts = zip as [1..]

roseTreeEqList :: Eq a => [a] -> RoseTree a -> Bool
roseTreeEqList [a] (RoseTree r []) = a==r
roseTreeEqList as (RoseTree r rs)  = all (roseTreeEqList (delete r as)) rs

prop_allTop_unchanging :: [Int] -> Property
prop_allTop_unchanging as = ((length as <= 8) && (not $ elem 0 as)) ==>
  roseTreeEqList as $ allTop ((1,0),[]) $
    dagHelper (\x y -> 0 == x `mod` y) (zip as [1..]) []

{- Tests -}
test0 :: IO Bool
test0 = checkMatches "Test/fixture/strategies/helloWorld_student.java" "Test/fixture/strategies/helloWorld_model.java"

test1 :: IO Bool
test1 = matchesItself "Test/fixture/strategies/helloWorld_student.java"

test2 :: IO Bool
test2 = selfIsLeftmost "Test/fixture/strategies/helloWorld_student.java"

test3 :: IO Bool
test3 = selfIsLeftmost "Test/fixture/strategies/wide.java"

allTests :: TestTree
allTests = testGroup "Strategies tests"
  [ testCase "helloWorld"                $ assert test0
  , testCase "matchesItself"             $ assert test1
  , testCase "selfIsLeftmost_helloWorld" $ assert test2
  , testCase "selfIsLeftmost_wide"       $ assert test3
  , testProperty "prop_dagHelper_unchanging" prop_dagHelper_unchanging
  , testProperty "prop_allTop_unchanging"    prop_allTop_unchanging
  ]
