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

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- NormTestUtil provides utilities for testing normalizers.
module Norm.NormTestUtil (
  -- ** Re-exports
    module RE
  -- ** Operations
  , normFixture
  , normTest
  , normTestsDir
  , normTestDir
  , normTester
  ) where

import Data.List (intercalate)

import TestUtil    as RE
import Norm.NormCS as RE

import CoreS.Parse (parseConv)
import CoreS.ConvBack (prettyCore')

-- | Prefix a test fixture with the test/norm fixture directory.
normFixture :: FilePath -> FilePath
normFixture = fixturePrefix . ("norm" </>)

-- | Construct a test group of normalization tests for the given
-- module mod, and the directory dir, and the set of NormalizerCUs norms.
-- See 'normTestDir' for more details.
normTestsDir :: TestName -> FilePath -> [NormalizerCU] -> TestTree
normTestsDir mod dir norms =
  let name  = mod ++ " tests"
      fun i = normTestDir (intercalate "_" [dir, "unittest", show i]) dir i
  in  testGroup name $ zipWith fun [1..] norms

-- | Constructs a normalization test given:
-- 1) the name of the test,
-- 2) the relative directory <DIR> of the test files,
-- 3) the <NUMBER> of the test,
-- 4) the normalizers to run on the to-test file.
--
-- The test is successful if the golden AST and the normalized to-test AST
-- are identical.
--
-- The path of the golden file will be:
-- > Test/fixture/norm/<DIR>/Expected<NUMBER>.java
--
-- The path of the to-test file will be:
-- > Test/fixture/norm/<DIR>/Source<NUMBER>.java
normTestDir :: TestName -> FilePath -> Int -> NormalizerCU -> TestTree
normTestDir name dir n = normTest name (sn "Expected") (sn "Source")
  where sn x = dir </> x ++ show n

-- | Constructs a normalization test given:
-- 1) the name of the test,
-- 2) the <PATH> to the golden file:  Test/fixture/norm/<PATH>,
-- 3) the <PATH> to the to-test file: Test/fixture/norm/<PATH>,
-- 4) the normalizers to run on the to-test file.
--
-- The test is successful if the golden AST and the normalized to-test AST
-- are identical.
normTest :: TestName -> FilePath -> FilePath -> NormalizerCU -> TestTree
normTest name expected src normalizers = goldenFileE name
  (normFixture $ expected <.> "java")
  (normFixture $ src      <.> "java")
  (normTester normalizers)

-- | Given a golden and a to-test Java concrete syntax tree (CST),
-- and a Normalizer to execute on the to-test CST,
-- tests if the normalizer, executed on the to-test AST,
-- produces an identical AST to the golden.
normTester :: NormalizerCU -> String -> String -> Either String ()
normTester norm gCST tCST = do
  (gAST :: CompilationUnit) <- parseConv gCST
  (tAST :: CompilationUnit) <- parseConv tCST
  normEqOrFail norm gAST gAST $ \gCSTn ->
      [ "FAILURE: Normalization on golden AST is not identity!"
      , "golden CST:", gCST
      , "golden CST, normalized:", gCSTn
      ]
  normEqOrFail norm gAST tAST $ \tCSTn ->
      [ "FAILURE: Normalized AST does not match golden."
      , "golden CST:", gCST
      , "test CST:", tCST
      , "test CST, normalized:", tCSTn
      ]

normEqOrFail :: NormalizerCU -> CompilationUnit -> CompilationUnit
             -> (String -> [String]) -> Either String ()
normEqOrFail norm gAST tAST onErr =
  let tASTn = executeNormalizer norm tAST
  in unless (gAST == tASTn) $ prettyCore' tASTn >>= Left . unlines . onErr