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

{-# LANGUAGE LambdaCase, TemplateHaskell, DeriveFunctor #-}

-- | A DTree is a rose tree based on difference lists.
-- This can be useful during construction of a rose tree.
module Data.DTree (
  -- * Data types
    DTree (..)
  -- * To/From: Data.Tree
  , fromTree
  , toTree
  -- * Construction
  , dtLeaf
  , dtSnoc
  , dtSnocL
  , dtCons
  , dtConsL
  ) where

import Data.Tree (Tree (..))
import Data.DList (DList, cons, snoc, empty, toList, fromList)

data DTree a = DTree { dRoot :: a, dChildren :: DList (DTree a) }
  deriving (Eq, Ord, Show, Read, Functor)

-- | Convert from Data.Tree.Tree
fromTree :: Tree a -> DTree a
fromTree (Node r sf) = DTree r (fmap fromTree $ fromList sf)

-- | Convert to Data.Tree.Tree
toTree :: DTree a -> Tree a
toTree (DTree r sf) = Node r (fmap toTree $ toList sf)

-- | Construct a leaf with the given root label.
dtLeaf :: a -> DTree a
dtLeaf r = DTree r empty

-- | Snoc a tree into the children.
dtSnoc :: DTree a -> DTree a -> DTree a
dtSnoc c (DTree r sf) = DTree r $ snoc sf c

-- | Cons a tree into the children.
dtCons :: DTree a -> DTree a -> DTree a
dtCons c (DTree r sf) = DTree r $ cons c sf

-- | Snoc a leaf into the children.
dtSnocL :: a -> DTree a -> DTree a
dtSnocL = dtSnoc . dtLeaf

-- | Cons a leaf into the children.
dtConsL :: a -> DTree a -> DTree a
dtConsL = dtCons . dtLeaf