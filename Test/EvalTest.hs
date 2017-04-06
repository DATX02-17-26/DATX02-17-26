module EvalTest where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Exception as Exc

import EvaluationMonad
import GenStrat
import SolutionContext
import NormalizationStrategies
import CoreS.AST
import qualified CoreS.ASTUnitype as AST
import CoreS.Parse
import Norm.AllNormalizations as ALL

normalize :: CompilationUnit -> CompilationUnit
normalize = executeNormalizer ALL.normalizations

normalizeUAST :: AST.AST -> AST.AST
normalizeUAST  = AST.inCore normalize

checkMatches :: FilePath -> [FilePath] -> IO (Maybe Bool)
checkMatches stud mods = do
  let paths = Ctx stud mods
  Just (Ctx stud mods) <- Exc.catch
                            (Just <$> resultEvalM ((fmap parseConv) <$> readRawContents paths))
                            ((\e -> return Nothing `const` (e :: Exc.ErrorCall)) {-:: Exc.ErrorCall -> IO (Maybe (SolutionContext (CConv (Repr t))))-})
  case stud of
    Left _     -> return Nothing
    Right stud ->
      return $ Just $ or [ matches
                           normalizeUAST
                           (AST.toUnitype $ normalize stud)
                           (AST.toUnitype (normalize mod))
                         | (Right mod) <- mods ]

allTests :: TestTree
allTests = testGroup "Eval tests"
  [
   testCase "EVAL_hello_world_OK" $ assert test0
   , testCase "EVAL_SumNumbers_FAIL" $ assert test1
   , testCase "EVAL_Uppgift12a_2_OK" $ assert test2
  , testCase "EVAL_Uppgift12a_3_OK" $ assert (not <$> test3)
   , testCase "EVAL_Uppgift12a_4_FAIL" $ assert test4
  , testCase "EVAL_Uppgift12a_5_OK" $ assert test5
  ]

test :: FilePath -> [FilePath] -> IO Bool
test s ms = do
  matched <- checkMatches s ms
  case matched of
    Nothing -> return False
    Just a -> return a

test0 :: IO Bool
test0 = test "Test/fixture/strategies/helloWorld_student.java" ["Test/fixture/strategies/helloWorld_model.java"]

test1 :: IO Bool
test1 = not <$> test "Test/Student_solutions/sumNumbers0.java" ["Test/Student_solutions/sumNumbers1.java" ]

test2 :: IO Bool
test2 = test "Test/Eval/Uppgift12a_stud1.java"
   [
    "modelsolution/uppgift12a/Uppgift12a_1.java"
    , "modelsolution/uppgift12a/Uppgift12a_2.java"
    , "modelsolution/uppgift12a/Uppgift12a_3.java"
    ]

test3 :: IO Bool
test3 = test "Test/Eval/Uppgift12a_stud2.java"
           [
            "modelsolution/uppgift12a/Uppgift12a_1.java"
            , "modelsolution/uppgift12a/Uppgift12a_2.java"
            , "modelsolution/uppgift12a/Uppgift12a_3.java"
            ]

test4 :: IO Bool
test4 = not <$> test "Test/Eval/Uppgift12a_stud3.java"
                   [
                    "modelsolution/uppgift12a/Uppgift12a_1.java"
                    , "modelsolution/uppgift12a/Uppgift12a_2.java"
                    , "modelsolution/uppgift12a/Uppgift12a_3.java"
                    ]

test5 :: IO Bool
test5 = test "Test/Eval/Uppgift12a_stud4.java"
                   [
                      "modelsolution/uppgift12a/Uppgift12a_1.java"
                    , "modelsolution/uppgift12a/Uppgift12a_2.java"
                    , "modelsolution/uppgift12a/Uppgift12a_3.java"
                    ]
