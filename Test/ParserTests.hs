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

-- | Parser tests (on Language.Java.Parser)
module ParserTests (
    allTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Java.Parser as JP
import Language.Java.Syntax as JA

parserAssocRegression :: TestTree
parserAssocRegression = testCase "parser_associativity_regression_test" $
  JP.parser JP.exp "1 + 2 * 3" @?=
  Right (BinOp (Lit (Int 1)) Add (BinOp (Lit (Int 2)) Mult (Lit (Int 3))))

-- | All tests:
allTests :: TestTree
allTests = testGroup "Language.Java.Parser tests"
  [ parserAssocRegression
  ]