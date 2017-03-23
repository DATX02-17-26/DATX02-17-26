{- DATX02-17-26, automated assessment of imperative programs.
 - Copyright, 2017, see AUTHORS.md.
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

-- | Provides asorted utilities for testing.
module TestUtil (
    module RE
  , goldenFileM
  , goldenFileET
  , goldenFileE
  , fixturePrefix
  ) where

import Control.Monad.Except (ExceptT, runExceptT)

import System.FilePath as RE
import Test.Tasty      as RE
import Test.Tasty.Golden.Advanced

-- | Runs a golden test given:
-- 1) test name,
-- 2) the path to the golden file,
-- 3) the path to the to-test file,
-- 4) and then a function that is given the contents of 1), 2) and yields an
-- IO action that succeeds on Nothing or Just (error message inside) on failure.
goldenFileM :: TestName -> FilePath -> FilePath
            -> (String -> String -> IO (Maybe String))
            -> TestTree
goldenFileM n g t m = goldenTest n (readFile g) (readFile t) m (const $ pure ())

-- | Runs a golden test given:
-- 1) test name,
-- 2) the path to the golden file,
-- 3) the path to the to-test file,
-- 4) and then a function that is given the contents of 1), 2) and yields an
-- ExceptT IO action that succeeds on Right (value is ignored)
-- and Left (error message inside) on failure.
goldenFileET :: Show e
            => TestName -> FilePath -> FilePath
            -> (String -> String -> ExceptT e IO a)
            -> TestTree
goldenFileET n g t m = goldenFileM n g t $ \g' t' ->
  either (pure . show) (const Nothing) <$> runExceptT (m g' t')

-- | Runs a golden test given:
-- 1) test name,
-- 2) the path to the golden file,
-- 3) the path to the to-test file,
-- 4) and then a function that is given the contents of 1), 2) and yields an
-- Either value that succeeds on Right (value is ignored)
-- and Left (error message inside) on failure.
goldenFileE :: Show e
            => TestName -> FilePath -> FilePath
            -> (String -> String -> Either e a)
            -> TestTree
goldenFileE n g t m = goldenFileM n g t $ \g' t' ->
  pure $ either (pure . show) (const Nothing) (m g' t')

-- | Prefix a test fixture with the test fixture directory.
fixturePrefix :: FilePath -> FilePath
fixturePrefix fixture = "Test" </> "fixture" </> fixture