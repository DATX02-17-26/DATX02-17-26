module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified NormalizationTests as Norm

-- | HUnit unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "1 == 1" $ 1 @?= (1 :: Int)
  ]

-- | QuickCheck property based tests
propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ Norm.allTests
  ]

-- | The combination of property tests and unit tests
allTests :: TestTree
allTests = testGroup "All tests"
  [ propertyTests
  , unitTests
  ]

main :: IO ()
main = defaultMain allTests
