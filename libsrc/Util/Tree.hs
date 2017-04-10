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

{-# LANGUAGE LambdaCase, RankNTypes #-}

-- | Utilities for Data.Tree (rose trees):
module Util.Tree (
  -- ** Tree fusion
    (=*=)
  , fuseTree
  , tfuseF
  , tfuse
  -- ** Tree navigation
  , TreePath
  , atTree
  , atTree'
  , navTree
  , navTree'
  , treeParts
  , withTree
  , withTreeF
  , withTreeFP
  -- ** Reexports
  , module RE
  ) where

import Safe (atMay)
import Safe.Partial (Partial)
import Data.Function.Pointless ((.:))
import Data.Maybe (fromMaybe)
import Data.Tree as RE
import Data.Tuple (swap)
import Control.Arrow ((***), (&&&), second)
import Control.Monad ((>=>))

import Util.List (zipWithDef)

--------------------------------------------------------------------------------
-- Tree fusion:
--------------------------------------------------------------------------------

-- | Applies every function in the left tree to the elements in the right tree
-- and discards excess elements from any of the trees.
-- Can be used in conjunction with fmap to fuse trees with same structure.
-- > (+) <$> t1 =*= t2
(=*=) :: Tree (a -> b) -> Tree a -> Tree b
(=*=) (Node f fs) (Node a as) = Node (f a) $ zipWith (=*=) fs as
infixl 4 =*=

-- | Fuses together two trees with the user supplied function in
-- the first argument. Any excess elements from either tree is discarded.
fuseTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
fuseTree f t1 t2 = f <$> t1 =*= t2

-- | Fuses together two trees where the first is one with (a -> b) and
-- the second is one with a. In the case of the structure not being the same,
-- the default elements are used instead.
tfuseF :: (a, a -> b) -> Tree (a -> b) -> Tree a -> Tree b
tfuseF z (Node f fs) (Node a as) = Node (f a) $
  zipWithDef (((pure *** pure) . swap) z) (tfuseF z) fs as

-- | Fuses together two trees where the first is one with (a -> b)s and
-- the second is one with as. In the case of the structure not being the same,
-- the default elements are used instead.
tfuse :: (a, b) -> Tree (a -> b) -> Tree a -> Tree b
tfuse = tfuseF . second const

--------------------------------------------------------------------------------
-- Tree navigation:
--------------------------------------------------------------------------------

-- | A path / cursor into a rose tree.
type TreePath = [Int]

-- | Safely indexes into the n:th subforest of a tree, starting from 0.
atTree :: Tree a -> Int -> Maybe (Tree a)
atTree = atMay . subForest

-- | Partial version of 'atTree'.
atTree' :: Partial => Tree a -> Int -> Tree a
atTree' = fromMaybe (error "subForest too small") .: atTree

-- | Safely navigates into the decendant of the given tree using
-- atTree as the semantics for indexing at each level.
navTree :: Tree a -> TreePath -> Maybe (Tree a)
navTree = foldl (\mt i -> mt >>= (`atTree` i)) . pure

-- | Partial version of 'navTree''
navTree' :: Tree a -> TreePath -> Tree a
navTree' = fromMaybe (error "subForest too small") .: navTree

-- | Destruct a tree into its components.
treeParts :: Tree a -> (a, Forest a)
treeParts = rootLabel &&& subForest

-- | lift a binary function on the rootLabel and the subForest to a function
-- on just the rose tree.
withTree :: (x -> Forest x -> r) -> Tree x -> r
withTree f = uncurry f . treeParts

-- | lift a binary function on the rootLabel and a function indexing into
-- the root label of a subbranch to a function on just the rose tree.
withTreeF :: (x -> (Int -> Maybe x) -> r) -> Tree x -> r
withTreeF f = uncurry f . second (>=> pure . rootLabel) . (rootLabel &&& atTree)

-- | Partial version of withTreeF in the indexing function.
withTreeFP :: (x -> ({-Partial =>-} Int -> x) -> r) -> Tree x -> r
withTreeFP f = uncurry f . second (rootLabel .) . (rootLabel &&& atTree')