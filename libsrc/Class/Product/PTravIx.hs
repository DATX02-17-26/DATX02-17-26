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

{-# LANGUAGE TypeFamilies, RankNTypes #-}

-- | Product types that provide a Traversal' of indices corresponding
-- to the length of the list of components making up the product.
module Class.Product.PTravIx (
  -- * Classes
    PTravIx
  -- * Types
  , PIndex
  -- * Primitive operations
  , pTravIx
  ) where

import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Control.Lens (Traversal', Traversal)

import Class.Product.Uncons
import Class.Product.Flatten

--------------------------------------------------------------------------------
-- Class:
--------------------------------------------------------------------------------

-- | Product types that provide a Traversal' of indices corresponding
-- to the length of the list of components making up the product.
class PTravIx p where
  -- | The indices type.
  type PIndex p i :: *

  -- | Provides the Traversal'.
  pTravIx :: [i] -> Traversal' p (PIndex p i)

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

-- | Helper for making instances.
inst :: Maybe t -> Traversal s s t b
inst mit = fromMaybe (const pure) $ do
  it <- mit
  pure $ \f x -> const x <$> f it

mkinst :: (UnconsL h e, HFlatten h t, Applicative f)
       => [e] -> (t -> f b) -> s -> f s
mkinst = inst . fmap fst . unconsF

instance PTravIx (x0, x1) where
  type PIndex    (x0, x1) i = F2 i
  pTravIx is0 = inst $ fst <$> unconsL is0

instance PTravIx (x0, x1, x2) where
  type PIndex    (x0, x1, x2) i = F3 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3) where
  type PIndex    (x0, x1, x2, x3) i = F4 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3, x4) where
  type PIndex    (x0, x1, x2, x3, x4) i = F5 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3, x4, x5) where
  type PIndex    (x0, x1, x2, x3, x4, x5) i = F6 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3, x4, x5, x6) where
  type PIndex    (x0, x1, x2, x3, x4, x5, x6) i = F7 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3, x4, x5, x6, x7) where
  type PIndex    (x0, x1, x2, x3, x4, x5, x6, x7) i = F8 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3, x4, x5, x6, x7, x8) where
  type PIndex    (x0, x1, x2, x3, x4, x5, x6, x7, x8) i = F9 i
  pTravIx = mkinst

instance PTravIx (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) where
  type PIndex    (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) i = F10 i
  pTravIx = mkinst