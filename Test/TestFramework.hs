{-# LANGUAGE RecordWildCards #-}
module TestFramework where

import System.Exit
import Test.QuickCheck

-- | Test a property with QuickCheck and fail if it fails
testQuickCheck :: (Testable p) => Args -> p -> IO ()
testQuickCheck args p = do
  r1 <- quickCheckWithResult args p
  case r1 of
    Failure {..} -> exitFailure
    _            -> return ()
