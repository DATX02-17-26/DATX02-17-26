{-# LANGUAGE TypeApplications #-}

module RoseGenExamples where

import Util.RoseGen
import Control.Monad

example0 :: RoseGen (Int, [Int])
example0 = do
  n <- anything @Int `suchThat` (\x -> -1 < x && x < 5)
  list <- replicateM n $ ((abs <$> anything @Int) `suchThat` (<5))
  return (n, list)
