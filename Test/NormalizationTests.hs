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

module NormalizationTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Lens

import Norm.NormM
import NormalizationStrategies

import qualified Norm.VarDeclTest        as NoVD
import qualified Norm.NormForTest        as NoFo
import qualified Norm.ElimRedundantTest  as ElRe
import qualified Norm.ElimDeadTest       as ElDe
import qualified Norm.AlphaRTest         as AlRe
import qualified Norm.VarDeclTest        as NoVD
import qualified Norm.NormForTest        as NoFo
import qualified Norm.ElimRedundantTest  as ElRe
import qualified Norm.ElimDeadTest       as ElDe
import qualified Norm.IfElseEmptyTest    as IfEE
import qualified Norm.DoWToWhileTest     as DoWh
import qualified Norm.CompAssignmentTest as CoAs
import qualified Norm.ForIndexTest       as ForI
import qualified Norm.FloatToDoubleTest  as FlDo
import qualified Norm.StepOpTest         as StOp
import qualified Norm.SOPTest            as SofP
import qualified Norm.ForIndexCLTE       as FICL

allTests :: TestTree
allTests = testGroup "Normalization tests"
  [ normStrat
  , NoVD.allTests
  , NoFo.allTests
  , ElRe.allTests
  , ElDe.allTests
  , AlRe.allTests
  , IfEE.allTests
  , DoWh.allTests
  , CoAs.allTests
  , ForI.allTests
  , FlDo.allTests
  , SofP.allTests
  , StOp.allTests
  , FICL.allTests
  ]

normStrat :: TestTree
normStrat = testGroup "Normalization strategies tests"
  [ testProperty "prop_onlyStagesSubset"        prop_onlyStagesSubset
  , testProperty "prop_onlyStagesDoesNotForget" prop_onlyStagesDoesNotForget
  ]

prop_onlyStagesSubset :: Normalizer Int -> [Int] -> Bool
prop_onlyStagesSubset norm stages =
  and [ n `elem` stages | n <- allStages (onlyStages stages norm)]

prop_onlyStagesDoesNotForget :: Normalizer Int -> [Int] -> Bool
prop_onlyStagesDoesNotForget norm stages =
  and [(n `notElem` stages) || (n `elem` allStages norm) | n <- allStages norm]
