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

{-|
Module      : Util.Function
Description : General utility for functions and arrows.
Copyright   : (c) Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

General utility for functions and arrows.
-}
module Util.Function (
    -- * Operations
    untilMatch, untilEq, applyN
  , (-|), (|-), (|=), (=|)
) where

--------------------------------------------------------------------------------
-- credit: previous work in "Compiler Construction" by Mazdak, 2016.
--------------------------------------------------------------------------------

-- | 'untilMatch': applies the second argument which is the function f, starting
-- with the value in the third argument, which is x, and replaces it with that
-- one, until the first argument which is a binary predicate, p, yields false
-- when given the last accepted x and f x.
-- This function is a generalization of 'until'
untilMatch :: (a -> a -> Bool) -> (a -> a) -> a -> a
untilMatch p f x
    | p x x'    = x
    | otherwise = untilMatch p f x'
    where x' = f x

-- | 'untilEq': specialization of 'untilMatch' for '(==)'
untilEq :: Eq a => (a -> a) -> a -> a
untilEq = untilMatch (==)

-- | 'applyN': applies a function f, i times to x. Therefore applyN 0 id == id.
applyN :: (Eq i, Num i) => i -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN i f x = applyN (i - 1) f $ f x

--------------------------------------------------------------------------------
-- credit: http://stackoverflow.com/a/6358630/1063961
--------------------------------------------------------------------------------

-- | Left associative flipped function application.
(-|) :: a -> (a -> b) -> b
(-|) = flip ($)

-- | Left associative function application.
(|-) :: (a -> b) -> a -> b
(|-) = ($)

-- | Right associative flipped function application of binary function.
(|=) :: (a -> b -> c) -> b -> a -> c
(|=) = flip

-- | Right associative function application.
(=|) :: a -> (a -> c) -> c
(=|) = (-|)

infixl 3 -|, |-
infixr 3 =|, |=