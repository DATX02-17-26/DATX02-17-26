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
   NormalizationStrategy
 , NormalizationRule
 , Normalizer
 , execute
 , name
 , stages
 , makeRule
 , (><)
 , (<>)
 , include
 , ignore
 , ignoreStages
 , allStages
 , onlyStages
 , executeNormalizer
 ) where

import Data.List (nub, intersect, union, (\\), sort)
import Control.Monad (foldM)
import Control.Lens (each, (%~))

import Util.TH (deriveLens)

import Norm.NormM

-- | In what stage(es) does a normalization rule execute
type NormalizationStages = [Int]

-- | A rule for how to normalize an expression of type a
data NormalizationRule a = NormR
  { _execute :: NormArr a
  , _name    :: String
  , _stages  :: NormalizationStages
  }

-- | Construct a normalization rule
makeRule :: NormArr a -> String -> NormalizationStages -> NormalizationRule a
makeRule = NormR

-- | Make obligatory lenses
$(deriveLens [''NormalizationRule])

-- | Two rules are equal if they have the same name
instance Eq (NormalizationRule a) where
  l == r = _name l == _name r

-- | A `Normalizer` is just a list of rules
type Normalizer a = [NormalizationRule a]

-- | A normalization strategy is a way of discriminating rules
type NormalizationStrategy a = Normalizer a -> Normalizer a

-- | A binary operator on NormalizationStrategy:s.
type BinNS a = NormalizationStrategy a -> NormalizationStrategy a
            -> NormalizationStrategy a

-- | Take the intersection of two strategies
(><) :: BinNS a
f >< g = \a -> nub $ f a `intersect` g a

-- | Take the union of two strategies
(<>) :: BinNS a
f <> g = \a -> nub $ f a `union` g a

-- | A strategy which includes a list of rules
include :: [NormalizationRule a] -> NormalizationStrategy a
include rules = nub . (++ rules)

-- | A strategy which ignores a list of rules
ignore :: [NormalizationRule a] -> NormalizationStrategy a
ignore rules = filter (`elem` rules)

-- | Ignore a list of stages.
ignoreStages :: [Int] -> NormalizationStrategy a
ignoreStages xs = (each . stages) %~ (\\ xs)

-- | Execute only a list of stages.
onlyStages :: [Int] -> NormalizationStrategy a
onlyStages xs = (each . stages) %~ intersect xs

-- | Execute only a specific stage.
pickStage :: Int -> NormalizationStrategy a
pickStage stage = filter ((stage `elem`) . _stages)

-- | Execute a stage of a normalizer
executeNormalizerStage :: Normalizer a -> Int -> a -> a
executeNormalizerStage inputRules stage =
    normLoop $ thread $ pickStage stage inputRules

-- | Thread an `a` through a normalizer,
-- returns `(Change, a)'` if `a` normalized to `a'` and
-- `(Unique, a)` if no rules in the normalizer applied to `a`.
thread :: Normalizer a -> NormArr a
thread = flip $ foldM $ flip _execute

-- | Execute a normalizer on an `a`
executeNormalizer :: Normalizer a -> a -> a
executeNormalizer norm a = foldl (flip $ executeNormalizerStage norm)
                                 a
                                 (allStages norm)

-- | Obtain all stages present in a normalizer.
allStages :: Normalizer a -> [Int]
allStages = sort . nub . concatMap _stages
