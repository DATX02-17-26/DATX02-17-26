module NormalizationTests where
import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Lens

import Norm.NormM
import NormalizationStrategies

instance Show (NormalizationRuleT m a) where
  show = (^. name)

allTests :: TestTree
allTests = testGroup "Normalization strategies tests"
  [ testProperty "prop_onlyStagesSubset"        prop_onlyStagesSubset
  , testProperty "prop_onlyStagesDoesNotForget" prop_onlyStagesDoesNotForget
  ]

prop_onlyStagesSubset :: Normalizer Int -> [Int] -> Bool
prop_onlyStagesSubset norm stages =
  and [ n `elem` stages | n <- allStages (onlyStages stages norm)]

prop_onlyStagesDoesNotForget :: Normalizer Int -> [Int] -> Bool
prop_onlyStagesDoesNotForget norm stages =
  and [(n `notElem` stages) || (n `elem` allStages norm) | n <- allStages norm]
