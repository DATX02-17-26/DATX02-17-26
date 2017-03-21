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

{- Javalette Compiler, a simple C like language.
 - Copyright, 2016, Mazdak Farrokhzad
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
Module      : Class.Sizeables
Description : Provides the classes Growable, Shrinkable, Zero and related ops.
Copyright   : (c) Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Provides the classes Growable, Shrinkable, Zero and related ops.
They, together with Ord, form a looser coupled version of Enum.
-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Class.Sizeables (
    -- * Classes
    Growable, Shrinkable, Zero,

    -- * Primitive operations
    zero, isZero, notZero, grow, shrink,

    -- * Auxilary operations
    growN, growZN, shrinkN, shrinkZN, shrinkCZ, growInf, shrinkInf,
    growThenInf, shrinkThenInf, growThenUntil, shrinkThenUntil,
) where

import Util.Function

--------------------------------------------------------------------------------
-- Classes:
--------------------------------------------------------------------------------

{-| 'Growable' is the class of types that can be grown. If a type is also
    'Bounded', then 'grow' 'maxbound' should result in a runtime error.

    For example:
    > data Nat = Z | S Nat deriving Eq
    > instance Zero Nat where zero = Z
-}
class Growable x where
    -- | 'grow': the successor of a value.
    grow :: x -> x

{-| 'Shrinkable' is the class of types that can be shrunk. If a type is also
    'Bounded', then 'shrink' 'minbound' should result in a runtime error.

    For example:
    > Shrinkable Nat where shrink Z = Z; shrink (S x) = x
-}
class Shrinkable x where
    -- | 'shrink': the predecessor of a value.
    shrink :: x -> x

{-| 'Zero' is the class of types for which a natural 'zero' is defined.

    For example:
    > instance Zero Nat where zero = Z

    We have that for any instance
    > (Zero x, Growable x, Shrinkable x, Eq x) => Enum x

    For example
    @
        instance Enum Nat where
        toEnum         = growZN
        fromEnum       = shrinkCZ
        succ           = grow
        pred           = shrink
        enumFrom       = growInf
        enumFromThen   = growThenInf
        enumFromTo     = growUntil
        enumFromThenTo = growThenUntil
    @
-}
class Zero x where
    -- | 'zero': the natural zero.
    zero :: x

--------------------------------------------------------------------------------
-- Auxilary operations:
--------------------------------------------------------------------------------

-- | 'isZero': yields true if value is 'zero'.
isZero :: (Zero x, Eq x) => x -> Bool
isZero = (== zero)

-- | 'notZero': yields true if value is not 'zero'.
notZero :: (Zero x, Eq x) => x -> Bool
notZero = (/= zero)

-- | 'growN': 'grow' a value n times.
growN :: Growable x => Int -> x -> x
growN = flip applyN grow

-- | 'growZN': 'grow' 'zero' n times. Can be used as 'toEnum'.
growZN :: (Zero x, Growable x) => Int -> x
growZN = flip growN zero

-- | 'shrinkN': 'shrink' a value n times.
shrinkN :: Shrinkable x => Int -> x -> x
shrinkN = flip applyN shrink

-- | 'shrinkN': 'shrink' 'zero' n times.
shrinkZN :: (Shrinkable x, Zero x) => Int -> x
shrinkZN = flip shrinkN zero

-- | 'shrinkCZ': counts how many times a value can be shrunk until it is 'zero'.
-- Can be used as 'fromEnum'.
shrinkCZ :: (Zero x, Shrinkable x, Eq x) => x -> Int
shrinkCZ = length . takeWhile notZero . shrinkInf

-- | 'growInf': 'grow' a value an infinite number of times and produce the
-- intermediate results. Can be used as 'enumFrom'.
growInf :: Growable x => x -> [x]
growInf = iterate grow

-- | 'shrinkInf': 'shrink' a value an infinite number of times and produce the
-- intermediate results.
shrinkInf :: Shrinkable x => x -> [x]
shrinkInf = iterate shrink

-- | 'growThenInf': prepend 'growInf' with value. Can be used as 'enumFromThen'.
growThenInf :: Growable x => x -> x -> [x]
growThenInf x = (x :) . growInf

-- | 'shrinkThenInf': prepend 'shrinkInf' with value.
shrinkThenInf :: Growable x => x -> x -> [x]
shrinkThenInf x = (x :) . growInf

-- | 'growUntil': starting from a value, 'grow' until it is another.
-- Can be used as 'enumFromTo'.
growUntil :: (Growable x, Ord x) => x -> x -> [x]
growUntil x y | x > y     = []
              | otherwise = takeWhile (/= y) (growInf x) ++ [y]

-- | 'shrinkUntil': starting from a value, 'shrink' until it is another.
shrinkUntil :: (Shrinkable x, Ord x) => x -> x -> [x]
shrinkUntil x y | x < y     = []
                | otherwise = takeWhile (/= y) (shrinkInf x) ++ [y]

-- | 'growUntil': prepend 'growUntil' with a value.
-- Can be used as 'enumFromThenTo'.
growThenUntil :: (Growable x, Ord x) => x -> x -> x -> [x]
growThenUntil x y = (x :) . growUntil y

-- | 'growUntil': prepend 'growUntil' with a value.
-- Can be used as 'enumFromThenTo'.
shrinkThenUntil :: (Shrinkable x, Ord x) => x -> x -> x -> [x]
shrinkThenUntil x y = (x :) . shrinkUntil y