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

{-# LANGUAGE TemplateHaskell #-}

module NormalizationStrategies (
   NormalizationStrategyT
 , NormalizationRuleT
 , NormalizerT
 , NormalizationStrategy
 , NormalizationRule
 , Normalizer
 , execute
 , name
 , stages
 , makeRule
 , makeRule'
 , (><)
 , (<>)
 , include
 , ignore
 , ignoreStages
 , allStages
 , onlyStages
 , pickStage
 , executeNormalizerT
 , executeNormalizer
 , module X
 ) where

import Data.List (nub, intersect, union, (\\), sort)
import Control.Monad (foldM)
import Control.Lens (each, (%~), (^.))
import Data.Function (on)
import Data.Function.Pointless ((.:))
import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary)

import Util.TH (deriveLens)

import Norm.NormM as X

--------------------------------------------------------------------------------
-- Types, transformers:
--------------------------------------------------------------------------------

-- | In what stage(es) does a normalization rule execute.
type NormalizationStages = [Int]

-- | A rule for how to normalize an expression of type a.
data NormalizationRuleT m a = NormR
  { _execute :: NormArrT m a
  , _name    :: String
  , _stages  :: NormalizationStages
  }

-- | Construct a normalization rule
makeRule :: NormArrT m a -> String -> NormalizationStages -> NormalizationRuleT m a
makeRule = NormR

-- | Construct a normalization rule with the executing logic as last argument.
makeRule' :: String -> NormalizationStages -> NormArrT m a -> NormalizationRuleT m a
makeRule' n s e = makeRule e n s

-- | Make obligatory lenses.
$(deriveLens [''NormalizationRuleT])

instance Show (NormalizationRuleT m a) where
  show = (^. name)

-- | A `NormalizerT` is just a list of rules.
type NormalizerT m a = [NormalizationRuleT m a]

-- | A normalization strategy is a way of discriminating rules.
type NormalizationStrategyT m a = NormalizerT m a -> NormalizerT m a

-- | A binary operator on NormalizationStrategy:s.
type BinNS m a = NormalizationStrategyT m a -> NormalizationStrategyT m a
              -> NormalizationStrategyT m a

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance (Monad m, CoArbitrary a, Arbitrary a) =>
         Arbitrary (NormalizationRuleT m a) where
  arbitrary = makeRule <$> arbitrary <*> arbitrary <*> arbitrary

-- | Two rules are equal if they have the same name.
instance Eq (NormalizationRuleT m a) where
  (==) = (==) `on` _name

--------------------------------------------------------------------------------
-- Types, simplified:
--------------------------------------------------------------------------------

-- | A rule for how to normalize an expression of type a. Identity base monad.
type NormalizationRule a = NormalizationRuleT Identity a

-- | A `Normalizer` is just a list of rules.
type Normalizer a = [NormalizationRule a]

-- | A normalization strategy is a way of discriminating rules.
type NormalizationStrategy m a = NormalizerT m a -> NormalizerT m a

--------------------------------------------------------------------------------
-- Operations:
--------------------------------------------------------------------------------

-- | Take the intersection of two strategies.
(><) :: BinNS m a
f >< g = \a -> nub $ f a `intersect` g a

-- | Take the union of two strategies.
(<>) :: BinNS m a
f <> g = \a -> nub $ f a `union` g a

-- | A strategy which includes a list of rules.
include :: [NormalizationRuleT m a] -> NormalizationStrategyT m a
include rules = nub . (++ rules)

-- | A strategy which ignores a list of rules.
ignore :: [NormalizationRuleT m a] -> NormalizationStrategyT m a
ignore rules = filter (`elem` rules)

-- | Ignore a list of stages.
ignoreStages :: [Int] -> NormalizationStrategyT m a
ignoreStages xs = (each . stages) %~ (\\ xs)

-- | Execute only a list of stages.
onlyStages :: [Int] -> NormalizationStrategyT m a
onlyStages xs = (each . stages) %~ intersect xs

-- | Execute only a specific stage.
pickStage :: Int -> NormalizationStrategyT m a
pickStage stage = filter ((stage `elem`) . _stages)

-- | Execute a stage of a normalizer.
executeNormalizerStage :: Monad m => NormalizerT m a -> Int -> a -> m a
executeNormalizerStage = normLoop . thread .: flip pickStage

-- | Thread an `a` through a normalizer,
-- returns `(Change, a)'` if `a` normalized to `a'` and
-- `(Unique, a)` if no rules in the normalizer applied to `a`.
thread :: Monad m => NormalizerT m a -> NormArrT m a
thread = flip $ foldM $ flip _execute

-- | Execute a normalizer on an `a`
executeNormalizerT :: Monad m => NormalizerT m a -> a -> m a
executeNormalizerT n a = foldM (flip $ executeNormalizerStage n) a (allStages n)

-- | Execute a normalizer on an `a`
executeNormalizer :: (Monad m, Comonad m) => NormalizerT m a -> a -> a
executeNormalizer = extract .: executeNormalizerT

-- | Obtain all stages present in a normalizer.
allStages :: NormalizerT m a -> [Int]
allStages = sort . nub . concatMap _stages