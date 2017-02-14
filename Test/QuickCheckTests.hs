module Main where
import Test.QuickCheck

import TestFramework

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  testQuickCheck stdArgs (\xs ys -> reverse ((xs :: [Int]) ++ ys) == (reverse xs) ++ (reverse ys))
