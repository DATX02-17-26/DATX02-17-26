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

-- | A FTree is a rose tree represented by opening and closing tags.
-- This can be useful in constructing trees with a writer.
module Data.FTree (
  -- * Data types
    FTPart (..)
  , FTree
  , FTreeD
  , unflat
  , toFlat
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..), cons, uncons)
import Util.List (mhead)
import Util.Monad (toMaybe)
import Data.Tree (Tree (..))
import Data.DList (DList, singleton, toList)
import Data.DTree (DTree, dtSnocL, dtSnoc, dtLeaf, toTree)

import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.State  (StateT, modify, get, put, execStateT)
import Control.Monad.Except (Except, throwError, runExcept)

--------------------------------------------------------------------------------
-- Data types:
--------------------------------------------------------------------------------

-- | Encoding of a building block of a rose tree.
data FTPart a
  = FTUnit { _ftLab :: a }
  | FTOpen { _ftLab :: a }
  | FTClose
  deriving (Eq, Ord, Show, Read)

-- | Encoding of a rose tree in a flat manner with a difference list.
type FTreeD a = DList (FTPart a)

-- | Encoding of a rose tree in a flat manner.
type FTree a = [FTPart a]

--------------------------------------------------------------------------------
-- Conversion to Data.Tree:
--------------------------------------------------------------------------------

-- | Un-flatten a flat representation of a rose tree into Data.Tree.
-- The first argument is the root of the tree.
unflat :: Foldable t => a -> t (FTPart a) -> Maybe (Tree a)
unflat root ft = let comp  = mapM_ ftPartComp ft
                     state = runExcept $ execStateT comp (entryStack root)
                  in toTree . NE.head <$> toMaybe state

-- | A testing tree:
ftreeTest :: FTree String
ftreeTest = [
      FTUnit "a"
    , FTUnit "b"
    , FTOpen "c"
      , FTUnit "d"
      , FTOpen "e"
        , FTUnit "f"
        , FTUnit "g"
      , FTClose
    , FTClose
    , FTUnit "h"
    ]

type TStack a = NE.NonEmpty (DTree a)
type TComp  a = StateT (TStack a) (Except ()) ()

ftPartComp :: FTPart a -> TComp a
ftPartComp = \case
  FTUnit a -> modify $ mhead $ dtSnocL a
  FTOpen a -> modify $ cons (dtLeaf a)
  FTClose  -> uncons <$> get >>= \(h, m) ->
                maybe (throwError ()) (put . (mhead $ dtSnoc h)) m

entryStack :: a -> TStack a
entryStack = (:| []) . dtLeaf

--------------------------------------------------------------------------------
-- Conversion from Data.Tree:
--------------------------------------------------------------------------------

-- | Flatten the tree.
toFlat :: Tree a -> FTree a
toFlat t = toList $ execWriter $ flattenH t

flattenH :: Tree a -> Writer (FTreeD a) ()
flattenH (Node a sf) = case sf of
  [] -> tell (singleton $ FTUnit a)
  _  -> do
    tell (singleton $ FTOpen a)
    mapM_ flattenH sf
    tell (singleton $ FTClose)