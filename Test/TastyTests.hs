module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.Java.Parser as JP
import Language.Java.Syntax as JA

-- | HUnit unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "1 == 1" $ 1 @?= (1 :: Int)
  , testCase "parser" $
        JP.parser JP.exp "1 + 2 * 3" @?=
        Right (BinOp (Lit (Int 1)) Add (BinOp (Lit (Int 2)) Mult (Lit (Int 3))))
  ]

-- | QuickCheck property based tests
propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "propReverse" $ \xs ys -> reverse (xs ++ ys) == reverse (ys :: [Int]) ++ reverse xs
  ]

-- | The combination of property tests and unit tests
allTests :: TestTree
allTests = testGroup "All tests"
  [ propertyTests
  , unitTests
  ]

main :: IO ()
main = defaultMain allTests
