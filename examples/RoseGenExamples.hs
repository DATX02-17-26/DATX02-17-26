module RoseGenExamples where

import Util.RoseGen
import Control.Monad

example0 :: RoseGen (Int, [Int])
example0 = do
  n <- anything `suchThat` (\x -> 0 <= x && x <= 5)
  list <- replicateM n $ anything `suchThat` (\x -> 0 <= x && x <= 5)
  return (n, list)

example1 :: RoseGen (Int, Int)
example1 = do
  n' <- abs <$> anything
  n  <- abs <$> anything
  return (n, n')



