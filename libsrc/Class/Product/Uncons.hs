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

-- | Unconsing lists an arbitrarily many times.
module Class.Product.Uncons where

import Data.List (uncons)
import Control.Arrow (first)
import Control.Monad ((>=>))

import Class.Product.HPair
import Class.Product.Flatten

--------------------------------------------------------------------------------
-- Classes:
--------------------------------------------------------------------------------

-- | Uncons of a list to an arbitrarily many pairs associating to the left.
-- Uses an inductive definition based on the "previous" instance of the class.
--
-- Unconsing many elements is a all or nothing proposition,
-- if the list is to small to get p back, you get Nothing.
class UnconsL p e where
  -- | uncons an arbitrary many times, associating to the left.
  unconsL  ::     [e] -> Maybe (     p, [e])

  -- | uncons, but with the list passed in snd.
  -- We could add a constraint: UnconsL z e to enforce the induction.
  -- But not doing so adds power and let's the function
  -- be used for other purposes.
  unconsLZ :: (z, [e]) -> Maybe ((z, p), [e])
  unconsLZ (z, xs) = first (z,) <$> unconsL xs

-- | Unconses a list according to UnconsL and then flattens the product.
unconsF :: (HFlatten p c, UnconsL p e) => [e] -> Maybe (c, [e])
unconsF = unconsL >=> pure . first hflatten

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance UnconsL (L0  a) a where unconsL = pure . ((),)
instance UnconsL (L1  a) a where unconsL = uncons
instance UnconsL (L2  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L3  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L4  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L5  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L6  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L7  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L8  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L9  a) a where unconsL = unconsL >=> unconsLZ
instance UnconsL (L10 a) a where unconsL = unconsL >=> unconsLZ