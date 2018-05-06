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

{-# LANGUAGE LambdaCase #-}

-- | List utility functions.
module Util.List (
  -- ** Head manipulation
    mhead
  -- ** Permutations
  , countEq
  , countBy
  , isPerm
  , isPermEq
  -- ** Monadic
  , traverseMay
  -- ** Zipping
  , zipWithMay
  , zipWithDef
  , zipRem
  , zipRemWith
  -- ** Modules
  , module RE
  ) where

import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sort)
import Data.List as RE
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow (first)

--------------------------------------------------------------------------------
-- Head manipulation:
--------------------------------------------------------------------------------

-- | A container with a notion of a head.
class WithHead f where
  -- | Modifies the head of the container with the given function.
  mhead :: (a -> a) -> f a -> f a

instance WithHead NonEmpty where
  mhead f (x :| xs) = (f x :| xs)

instance WithHead [] where
  mhead f = \case
    []       -> []
    (x : xs) -> (f x : xs)

--------------------------------------------------------------------------------
-- Permutations:
--------------------------------------------------------------------------------

-- | Count the occurences of the first argument in the second using (==).
-- This assumes the given foldable is of finite length.
countEq :: (Foldable f, Eq a) => a -> f a -> Integer
countEq = countBy (==)

-- | Count the occurences of the second argument in the third using the user
-- supplied binary predicate in the first argument.
-- This assumes the given foldable is of finite length.
countBy :: Foldable f => (a -> a -> Bool) -> a -> f a -> Integer
countBy eq xt = foldl' (\c x -> c + if eq x xt then 1 else 0) 0

-- | Yields True if the first argument is a permutation of the second, using
-- (==) for equality. Worst case: O(n^2).
-- This assumes the given foldable is of finite length.
isPermEq :: (Foldable f, Eq a) => f a -> f a -> Bool
isPermEq xs ys = ((==) `on` length) xs ys &&
                 all (\x -> countEq x ys == countEq x xs) xs

-- | Yields True if the first argument is a permutation of the second,
-- this is done by sorting both. Worst case: O(max(n * log n, m log m)).
-- This assumes the given foldable is of finite length.
isPerm :: Ord a => [a] -> [a] -> Bool
isPerm = (==) `on` sort

--------------------------------------------------------------------------------
-- Monadic:
--------------------------------------------------------------------------------

-- | Traverses a list with a function yielding back Maybe b
-- and then keeps the Just values.
traverseMay :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
traverseMay = fmap catMaybes .: traverse

--------------------------------------------------------------------------------
-- Zipping:
--------------------------------------------------------------------------------

-- | 'zipWithMay' generalizes 'zipWith' by not dropping elements when
-- one list is smaller than the other, such as:
-- 
-- > zipWithMay f [1..] [1, 2]
-- 
-- or:
-- 
-- > zipWithMay f [1,2] [1..]
--
-- This is handled by using the Maybe monad.
--
-- Works on lists of infinite size.
-- If any of the given lists are infinite, then the resultant list will also be.
zipWithMay :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithMay f = curry $ \case
  ([], ys)     -> map (f      Nothing . Just) ys
  (xs, [])     -> map (flip f Nothing . Just) xs
  (x:xs, y:ys) -> f (Just x) (Just y) : zipWithMay f xs ys

-- | zipWithDef performs zipWith but with default elements for cases
-- where the lists are not of the same length.
zipWithDef :: (a, b) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDef z f = zipWithMay $ curry $ \m ->
  (fromMaybe . fst) z (fst m) `f` (fromMaybe . snd) z (snd m)

-- | zipWith that also yields the remaining list parts if there are any.
zipRemWith :: (a -> b -> c) -> [a] -> [b] -> ([c], ([a], [b]))
zipRemWith f = curry $ \case
  (a : as, b : bs) -> first (f a b :) $ zipRemWith f as bs
  ([],     b : bs) -> ([], ([], b : bs))
  (a : as, []    ) -> ([], (a : as, []))
  ([]    , []    ) -> ([], ([], []    ))

-- | zipRemWith specialized with (,).
zipRem :: [a] -> [b] -> ([(a, b)], ([a], [b]))
zipRem = zipRemWith (,)