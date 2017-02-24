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

module Util.List (
    countEq
  , countBy
  , isPerm
  , isPermEq
  ) where

import Data.Function (on)
import Data.List (sort)
import Data.Foldable (foldl')

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