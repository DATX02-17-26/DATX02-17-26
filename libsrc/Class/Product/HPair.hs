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

{-# LANGUAGE
    TupleSections
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

-- | Conversion class between left and right associative pair of pair of pair...
module Class.Product.HPair where

import Control.Arrow (first, second)

--------------------------------------------------------------------------------
-- Classes:
--------------------------------------------------------------------------------

-- | Conversion class between left and right associative pair of pair of pair ..
--
-- l and r must be isomorphic, and so:
-- > assocL . assocR = id
-- > assocR . assocL = id
--
-- Example:
-- > assocL (1, (2, 3)) == ((1, 2), 3)
-- > assocR ((1, 2), 3) == (1, (2, 3))
class HPair l r where
  -- | Left associates a right-associated pair of pair on snd of pair on snd...
  assocL :: r -> l

  -- | Right associates a left-associated pair of pair on fst of pair on fst...
  assocR :: l -> r

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

type L0  a = ()
type L1  a = a
type L2  a = (L1 a, a)
type L3  a = (L2 a, a)
type L4  a = (L3 a, a)
type L5  a = (L4 a, a)
type L6  a = (L5 a, a)
type L7  a = (L6 a, a)
type L8  a = (L7 a, a)
type L9  a = (L8 a, a)
type L10 a = (L9 a, a)

type R0  a = ()
type R1  a = a
type R2  a = (a, R1 a)
type R3  a = (a, R2 a)
type R4  a = (a, R3 a)
type R5  a = (a, R4 a)
type R6  a = (a, R5 a)
type R7  a = (a, R6 a)
type R8  a = (a, R7 a)
type R9  a = (a, R8 a)
type R10 a = (a, R9 a)

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance HPair (L2 a) (R2 a) where
  assocL = id
  assocR = id

instance HPair (L3 a) (R3 a) where
  assocL (a,  as) = first  (a,  ) $ assocL as
  assocR (as, at) = second (, at) $ assocR as

instance HPair (L4 a) (R4 a) where
  assocL (a1, (a2, (a3, a4))) = (((a1, a2), a3), a4)
  assocR (((a1, a2), a3), a4) = (a1, (a2, (a3, a4)))

instance HPair (L5 a) (R5 a) where
  assocL (a1, (a2, (a3, (a4, a5)))) = ((((a1, a2), a3), a4), a5)
  assocR ((((a1, a2), a3), a4), a5) = (a1, (a2, (a3, (a4, a5))))

instance HPair (L6 a) (R6 a) where
  assocL (a1, (a2, (a3, (a4, (a5, a6))))) = (((((a1, a2), a3), a4), a5), a6)
  assocR (((((a1, a2), a3), a4), a5), a6) = (a1, (a2, (a3, (a4, (a5, a6)))))

instance HPair (L7 a) (R7 a) where
  assocL (a1, (a2, (a3, (a4, (a5, (a6, a7)))))) =
         ((((((a1, a2), a3), a4), a5), a6), a7)
  assocR ((((((a1, a2), a3), a4), a5), a6), a7) =
         (a1, (a2, (a3, (a4, (a5, (a6, a7))))))

instance HPair (L8 a) (R8 a) where
  assocL (a1, (a2, (a3, (a4, (a5, (a6, (a7, a8))))))) =
         (((((((a1, a2), a3), a4), a5), a6), a7), a8)
  assocR (((((((a1, a2), a3), a4), a5), a6), a7), a8) =
         (a1, (a2, (a3, (a4, (a5, (a6, (a7, a8)))))))

instance HPair (L9 a) (R9 a) where
  assocL (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, a9)))))))) =
         ((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9)
  assocR ((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9) =
         (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, a9))))))))

instance HPair (L10 a) (R10 a) where
  assocL (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, a10))))))))) =
         (((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10)
  assocR (((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10) =
         (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, a10)))))))))