module Util.ListTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Util.List

prop_isPerms :: [Int] -> [Int] -> Bool
prop_isPerms xs ys = isPerm xs ys == isPermEq ys xs


allTests :: TestTree
allTests = testGroup "Util.List tests"
  [ testProperty "prop_isPerms" prop_isPerms
  ]
