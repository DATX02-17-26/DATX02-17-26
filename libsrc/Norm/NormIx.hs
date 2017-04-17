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

{-# LANGUAGE LambdaCase, FlexibleContexts, RankNTypes #-}

-- | Utilities for writing normalizers with a read-only index.
module Norm.NormIx (
  -- * IndexedTraversal construction from list of indices
    withIndices
  -- * Bottom-up indexed traversals
  -- ** General
  , itransformMOf
  , itransformMOnOf
  -- ** Tree based
  , sfIx
  , rtTransformMOf
  , rtTransformMOnOf
  -- ** Tree based with Traversal's from Data
  , transformMU
  , transformMB
  -- ** Tree based with Traversal's from Data and a Tree producing function.
  , ixNormEveryT
  , ixNormEvery
  -- * Manipulations on the with-index function.
  , wtPrismR
  , wtPrism
  ) where

import Data.Data (Data)
import Data.Monoid ((<>))
import Data.List (uncons)
import Data.Tree (Tree (..), subForest, rootLabel)
import Data.Function.Pointless ((.^))
import Data.Functor.Compose (Compose (Compose))
import Control.Arrow (first)
import Control.Monad.Trans.State (state, runState)
import Data.Data.Lens (uniplate, biplate)
import Control.Lens (Traversal', IndexedTraversal', LensLike',
  indexed, imapMOf, auf, iso, _Unwrapping)

import Class.HasError (HasError, toEither)
import Class.Product.PTravIx (PTravIx, PIndex, pTravIx)
import Util.Tree (withTree)

--------------------------------------------------------------------------------
-- IndexedTraversal construction from list of indices:
--------------------------------------------------------------------------------

-- | Creates an IndexedTraversal using the given indices.
-- This is only for the immediate level.
withIndices :: Traversal' s a -> [i] -> IndexedTraversal' i s a
withIndices l is f x = fst $ auf (iso state runState . _Unwrapping Compose) l
  (\a -> maybe (pure a, []) (first (flip (indexed f) a)) . uncons)
  x is

--------------------------------------------------------------------------------
-- IndexedTraversal versions of transformMOf, etc.:
--------------------------------------------------------------------------------

-- | Generalized indexed version of transformMOf.
itransformMOf :: Monad m
              => (i -> IndexedTraversal' i a a)
              -> (i -> a -> m a) -> i -> a -> m a
itransformMOf trav f = let go i t = imapMOf (trav i) go t >>= f i in go

-- | Generalized indexed version of itransformMOnOf.
itransformMOnOf :: Monad m
  => (i -> IndexedTraversal' i s a)
  -> (i -> IndexedTraversal' i a a)
  -> (i -> a -> m a) -> i -> s -> m s
itransformMOnOf b l f i = imapMOf (b i) $ itransformMOf l f

--------------------------------------------------------------------------------
-- Bottom up traversals with a rose tree as indices.
--------------------------------------------------------------------------------

-- | Creates an indexed traversal with indices from the subForest of the given
-- rose tree. This structure must correspond to the given Traversal' s a,
-- or the modified result will be smaller.
sfIx :: Traversal' s a -> Tree i -> IndexedTraversal' (Tree i) s a
sfIx t = withIndices t . subForest

-- | Monadic bottom up indexed traversal of a structure with
-- indices from a rose tree that corresponds 1:1 with the thing to traverse.
-- Indexed version of transformMOf.
rtTransformMOf :: Monad m
          => Traversal' a a
          -> (Tree i -> a -> m a) -> Tree i -> a -> m a
rtTransformMOf t = itransformMOf $ sfIx t

-- | Monadic bottom up indexed traversal of a structure with
-- indices from a rose tree that corresponds 1:1 with the thing to traverse.
-- Indexed version of transformMOnOf.
rtTransformMOnOf :: Monad m
            => Traversal' s a -> Traversal' a a
            -> (Tree i -> a -> m a) -> Tree i -> s -> m s
rtTransformMOnOf b l = itransformMOnOf (sfIx b) (sfIx l)

-- | Indexed version of normEveryT using a rose tree for decoration (index).
transformMU :: (Monad m, Data a)
            => (Tree i -> a -> m a) -> Tree i -> a -> m a
transformMU = rtTransformMOf uniplate

-- | Indexed version of normEvery using a rose tree for decoration (index).
transformMB :: (Monad m, Data s, Data a)
            => (Tree i -> a -> m a) -> Tree i -> s -> m s
transformMB = rtTransformMOnOf biplate uniplate

ixNormEveryH :: (Monad m, HasError e n)
             => (k -> Tree i -> a -> m a) -> (a -> n (Tree i)) -> k -> a -> m a
ixNormEveryH g mkTree f a =
  either (const $ pure a)
         (flip (g f) a)
         (toEither $ mkTree a)

-- | Indexed version of normEveryT using a rose tree for decoration (index).
-- The rose tree is produced by a function given the top level term.
ixNormEveryT :: (HasError e n, Monad m, Data a)
            => (a -> n (Tree i))
            -> (Tree i -> a -> m a)
            -> a -> m a
ixNormEveryT = ixNormEveryH transformMU

-- | Indexed version of normEvery using a rose tree for decoration (index).
-- The rose tree is produced by a function given the top level term.
ixNormEvery :: (HasError e n, Monad m, Data s, Data a)
            => (s -> n (Tree i))
            -> (Tree i -> a -> m a)
            -> s -> m s
ixNormEvery = ixNormEveryH transformMB

--------------------------------------------------------------------------------
-- Manipulations on the with-index function.
--------------------------------------------------------------------------------

-- | lift a binary function on the rootLabel and a traversal on prefixes of the
-- rootLabels of the subForest to a function on just the rose tree.
-- The traversal can then be composed with an actual prism
-- to get as many indices from the subForest as the prism has elements.
wtPrismR :: (PTravIx p, Applicative f)
        => (LensLike' f p (PIndex p x) -> x -> r) -> Tree x -> r
wtPrismR = withTree . (. (pTravIx . fmap rootLabel))

-- | lift a binary function on the rootLabel and a traversal on prefixes of the
-- subForest to a function on just the rose tree.
-- The traversal can then be composed with an actual prism
-- to get as many indices from the subForest as the prism has elements.
wtPrism :: (PTravIx p, Applicative f)
        => (LensLike' f p (PIndex p (Tree x)) -> x -> r) -> Tree x -> r
wtPrism = withTree . (. pTravIx)