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
module NormalizationStrategies
       ( NormalizationStrategy
       , NormalizationRule
       , Normalizer
       , execute
       , name
       , stages
       , (><)
       , (<>)
       , include
       , ignore
       , ignoreStages
       , onlyStages
       , executeNormalizer
       )
where
import Control.Lens
import Data.List

-- | In what stage(es) does a normalization rule execute
type NormalizationStages = [Int]

-- | A rule for how to normalize an expression of type a
data NormalizationRule a = Norm { _execute :: a -> Maybe a
                                , _name    :: String
                                , _stages  :: NormalizationStages
                                }

-- | Make obligatory lenses
$(makeLenses ''NormalizationRule)

-- | Two rules are equal if they have the same name
instance Eq (NormalizationRule a) where
  l == r = _name l == _name r

-- | A `Normalizer` is just a list of rules
type Normalizer a = [NormalizationRule a]

-- | A normalization strategy is a way of discriminating rules
type NormalizationStrategy a = Normalizer a -> Normalizer a

-- | Take the intersection of two strategies
(><) :: NormalizationStrategy a -> NormalizationStrategy a -> NormalizationStrategy a
f >< g = \a -> nub $ (f a) `intersect` (g a)

-- | Take the union of two strategies
(<>) :: NormalizationStrategy a -> NormalizationStrategy a -> NormalizationStrategy a
f <> g = \a -> nub $ (f a) `union` (g a)

-- | A strategy which includes a list of rules
include :: [NormalizationRule a] -> NormalizationStrategy a
include rules = nub . (++ rules)

-- | A strategy which ignores a list of rules
ignore :: [NormalizationRule a] -> NormalizationStrategy a
ignore rules = filter (flip elem rules)

-- | Ignore a list of stages
ignoreStages :: [Int] -> NormalizationStrategy a
ignoreStages xs = (each . stages) %~ (flip (\\) xs)

-- | Execute only a list of stages
onlyStages :: [Int] -> NormalizationStrategy a
onlyStages xs = (each . stages) %~ (intersect xs)

-- | Execute a stage of a normalizer
executeNormalizerStage :: Normalizer a -> Int -> a -> a
executeNormalizerStage inputRules stage a = loop a
  where
    rules = filter (\r -> stage `elem` (_stages r)) inputRules 

    -- Loop until no rules apply
    loop a = case thread a rules of
      Nothing -> a
      Just a' -> loop a'

-- | Thread an `a` through a normalizer, returns `Just a'` if `a` normalized to `a'` and
--   `Nothing` if no rules in the normalizer applied to `a`
thread :: a -> Normalizer a -> Maybe a
thread = thread' False
  where
    thread' True a []            = Just a
    thread' False a []           = Nothing
    thread' hasChanged a (r:rls) = case _execute r a of
      Nothing -> thread' hasChanged a rls
      Just a' -> thread' True a' rls -- A normalization succeded

-- | Execute a normalizer on an `a`
executeNormalizer :: Normalizer a -> a -> a
executeNormalizer norm a = foldl (flip $ executeNormalizerStage norm) a (allStages norm)

-- | Obtain all stages presenting in a normalizer
allStages :: Normalizer a -> [Int]
allStages = sort . nub . concat . map _stages
