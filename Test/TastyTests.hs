module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "1 == 1" $ 1 @?= (1 :: Int)
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "propReverse" $ \xs ys -> reverse (xs ++ ys) == reverse (ys :: [Int]) ++ reverse xs
  ]

allTests :: TestTree
allTests = testGroup "All tests"
  [ propertyTests
  , unitTests
  ]

main :: IO ()
main = defaultMain allTests
