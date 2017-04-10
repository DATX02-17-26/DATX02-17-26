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

-- | Rose tree data structure, isomorphic to Data.Tree.
module Data.RoseTree where

import Data.Tree (Tree (..))
import Data.Monoid ((<>))
import Text.PrettyPrint hiding ((<>))

--------------------------------------------------------------------------------
-- Data type:
--------------------------------------------------------------------------------

-- | Create a Rose Tree Data Type
data RoseTree a = RoseTree
  { root     :: a            -- ^ The rootLabel of the tree.
  , branches :: [RoseTree a] -- ^ The subForest of the tree.
  }
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance Functor RoseTree where
  fmap f (RoseTree a rs) = RoseTree (f a) (fmap f <$> rs)

instance Applicative RoseTree where
  pure x = RoseTree x []

  (RoseTree f fs) <*> x@(RoseTree y ys) =
      RoseTree (f y) $ fmap (<*> x) fs <> fmap (f <$>) ys

instance Monad RoseTree where
  return = pure

  (RoseTree x xs) >>= k =
    let RoseTree y ys = k x in
      RoseTree y $ fmap (>>= k) xs <> ys

prettyRoseTree :: Show a => RoseTree a -> Doc
prettyRoseTree (RoseTree a rs) =
  text "-" <+>
  text (show a) $$
    nest 2 (vcat $ map prettyRoseTree rs)

instance Show a => Show (RoseTree a) where
  show = render . prettyRoseTree

--------------------------------------------------------------------------------
-- Auxilary functions:
--------------------------------------------------------------------------------

-- | Filter a rose tree based on a predicate, always leave
-- the root node in place.
filterTree :: RoseTree a -> (a -> Bool) -> RoseTree a
filterTree (RoseTree a trees) p =
  RoseTree a [filterTree t p | t@(RoseTree a' _) <- trees, p a']

prune :: Int -> Int -> RoseTree a -> RoseTree a
prune w 0 (RoseTree a _)     = RoseTree a []
prune w d (RoseTree a trees) =
  RoseTree a [prune w (d - 1) tree | tree <- take w trees]

-- | Convert to Data.Tree.
toTree :: RoseTree a -> Tree a
toTree t = Node (root t) (toTree <$> branches t)

-- | Convert from Data.Tree.
fromTree :: Tree a -> RoseTree a
fromTree t = RoseTree (rootLabel t) (fromTree <$> subForest t)