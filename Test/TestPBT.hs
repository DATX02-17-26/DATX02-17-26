
{-# LANGUAGE TypeApplications, FlexibleContexts #-}

module TestPBT where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad

import EvaluationMonad
import PropertyBasedTesting
import SolutionContext
import RunJavac
import qualified Util.RoseGen as RG
import InputMonad

{- Infrastructure -}
compDir :: FilePath 
compDir = "tmp"

test :: SolutionContext FilePath -> RG.RoseGen String -> IO Bool
test paths gen =
  executeEvalM defaultEnv $
  withTemporaryDirectory compDir $ do
    compileContext paths compDir
    runPBT compDir gen

{- Test Cases -}
test0 :: IO Bool
test0 = test
  (Ctx "Test/Student_solutions/helloWorld0.java" ["Test/Model_solutions/helloWorld0.java"])
  RG.anything

sumNumbers :: InputMonoid m => InputMonad m ()
sumNumbers = do
  n <- abs <$> anything @Int
  inp  $ show n
  void $ replicateM n $ anything @Int >>= inp . show

test1 :: IO Bool
test1 = test
  (Ctx "Test/Student_solutions/sumNumbers0.java" ["Test/Model_solutions/sumNumbers0.java"])
  (makeGenerator (sumNumbers @NewlineString))

test2 :: IO Bool
test2 = not <$> test
  (Ctx "Test/Student_solutions/sumNumbers1.java" ["Test/Model_solutions/sumNumbers0.java"])
  (makeGenerator (sumNumbers @NewlineString))

test3 :: IO Bool
test3 = test
  (Ctx "Test/PbtTest/student/PbtStud2OK.java" ["Test/PbtTest/model/PbtMod1.java"
  , "Test/PbtTest/model/PbtMod2.java"])
  (makeGenerator (sumNumbers @NewlineString))

test4 :: IO Bool
test4 =  test
  (Ctx "Test/PbtTest/student/PbtStud1Failjava" ["Test/PbtTest/model/PbtMod1.java"
  , "Test/PbtTest/model/PbtMod2.java"])
  (makeGenerator (sumNumbers @NewlineString))

allTests :: TestTree 
allTests = testGroup "PBT tests"
  [ testCase "helloWorld" $ assert test0
  , testCase "numbers-0"  $ assert test1
  , testCase "numbers-1"  $ assert test2
  , testCase "printFromArgs" $ assert test3
  , testCase "printFromScanner" $ assert test4
  ]
