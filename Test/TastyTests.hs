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

-- | Entry point for all tests, of all kinds.
module Main (
    main
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import CoreS.Parse

import Language.Java.Parser as JP
import Language.Java.Syntax as JA

import qualified ParserTests        as Parser
import qualified NormalizationTests as Norm
import qualified Util.ListTests     as UL
import qualified TestPBT            as PBT
import qualified TestStrategies     as Strat
import qualified EvalTest           as Eval

-- | All tests:
allTests :: TestTree
allTests = testGroup "All tests"
  [ UL.allTests
  , Parser.allTests
  , Norm.allTests
  , PBT.allTests
  , Strat.allTests
  , Eval.allTests
  ]

main :: IO ()
main = defaultMain allTests
