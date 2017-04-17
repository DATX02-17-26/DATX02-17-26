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
    FlexibleInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

-- | Flattening of homogeneous parametric product types that are nested pairs
-- into a flat structure.
module Class.Product.Flatten where

import Class.Product.HPair

--------------------------------------------------------------------------------
-- Classes:
--------------------------------------------------------------------------------

-- | Class for flattening a homogeneous parametric product type.
class HFlatten h f | f -> h where
  -- | Flattens h.
  hflatten :: h -> f

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

type F0  a = ()
type F1  a = a
type F2  a = (a, a)
type F3  a = (a, a, a)
type F4  a = (a, a, a, a)
type F5  a = (a, a, a, a, a)
type F6  a = (a, a, a, a, a, a)
type F7  a = (a, a, a, a, a, a, a)
type F8  a = (a, a, a, a, a, a, a, a)
type F9  a = (a, a, a, a, a, a, a, a, a)
type F10 a = (a, a, a, a, a, a, a, a, a, a)

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance HFlatten (L2 a) (F2 a) where
  hflatten = id

instance HFlatten (L3 a) (F3 a) where
  hflatten ((a1, a2), a3) = (a1, a2, a3)

instance HFlatten (L4 a) (F4 a) where
  hflatten (((a1, a2), a3), a4) = (a1, a2, a3, a4)

instance HFlatten (L5 a) (F5 a) where
  hflatten ((((a1, a2), a3), a4), a5) = (a1, a2, a3, a4, a5)

instance HFlatten (L6 a) (F6 a) where
  hflatten (((((a1, a2), a3), a4), a5), a6) = (a1, a2, a3, a4, a5, a6)

instance HFlatten (L7 a) (F7 a) where
  hflatten ((((((a1, a2), a3), a4), a5), a6), a7) = (a1, a2, a3, a4, a5, a6, a7)

instance HFlatten (L8 a) (F8 a) where
  hflatten (((((((a1, a2), a3), a4), a5), a6), a7), a8)
    = (a1, a2, a3, a4, a5, a6, a7, a8)

instance HFlatten (L9 a) (F9 a) where
  hflatten ((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9)
    = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance HFlatten (L10 a) (F10 a) where
  hflatten (((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10)
    = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)