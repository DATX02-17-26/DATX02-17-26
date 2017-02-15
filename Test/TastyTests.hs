module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | HUnit unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "1 == 2" $ 2 @?= (1 :: Int)
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
