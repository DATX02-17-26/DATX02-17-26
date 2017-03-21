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

{-# LANGUAGE ConstraintKinds #-}

-- | Functor, Applicative, Monad (+ transformer) utilities.
module Util.Monad (
  -- ** Classes and types
    MonadCo
  -- ** General utilities
  , (<$$>)
  , unless'
  , fkeep
  , untilEqM
  , untilMatchM
  -- ** Monadic folds and traversals
  , traverseJ
  -- ** Monadic sorting
  , MComparator
  , sortByM
  -- ** Monad stack transformations
  , rebase
  , io
  , exceptT
  ) where

import Control.Comonad (Comonad)
import Control.Monad (join, (>=>))
import Data.Function.Pointless ((.:))
import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (MFunctor, hoist, generalize)  
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (except)

--------------------------------------------------------------------------------
-- Classes and types:
--------------------------------------------------------------------------------

-- | Combined constraint of being both a Monad and a Comonad.
-- With great power come greats responsibility!
type MonadCo m = (Monad m, Comonad m)

--------------------------------------------------------------------------------
-- General utilities:
--------------------------------------------------------------------------------

-- | '<$$>': alias for composition of fmap with itself.
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

-- | 'unless'': sequentially composes first argument with a check where the
-- value is given to a predicate (in second argument). If the predicate holds,
-- then its given value is returned, else the function in the third argument is
-- given the value and is the result of the computation.
unless' :: Monad m => m a -> (a -> Bool) -> (a -> m a) -> m a
unless' m p e = m >>= \x -> if p x then return x else e x

-- | 'fkeep': given a function that produces f b given an a. And given an a in
-- the second argument, a functor with both values as a pair is produced.
fkeep :: Functor f => (a -> f b) -> a -> f (a, b)
fkeep f a = (\b -> (a, b)) <$> f a

-- | 'untilEqM': same as 'untilEq' but in a monadic context.
untilEqM :: (Eq a, Monad m) => (a -> m a) -> m a -> m a
untilEqM = untilMatchM (==)

-- | 'untilMatchM': same as 'untilMatch' but in a monadic context.
untilMatchM :: Monad m => (a -> a -> Bool) -> (a -> m a) -> m a -> m a
untilMatchM p f = (>>= \x -> unless' (f x) (p x) (untilMatchM p f . return))

--------------------------------------------------------------------------------
-- Monadic folds:
--------------------------------------------------------------------------------

-- | 'traverseJ': traverse and then join on the result.
traverseJ :: (Applicative f, Traversable t, Monad t)
             => (a -> f (t b)) -> t a -> f (t b)
traverseJ = fmap join .: traverse

--------------------------------------------------------------------------------
-- Monadic sorting:
--------------------------------------------------------------------------------

-- | A monadic comparator:
type MComparator m a = a -> a -> m Ordering

-- | Monadic sort, stable,
-- From: https://hackage.haskell.org/package/monadlist-0.0.2
sortByM :: Monad m => MComparator m a -> [a] -> m [a]
sortByM cmp = sequences >=> mergeAll
  where
    sequences (a:b:xs) =
      cmp a b >>= onGT (descending b [a] xs) (ascending b (a:) xs)
    sequences xs       = pure [xs]

    descending a as cs@(b:bs) =
      cmp a b >>= onGT (descending b (a:as) bs) (((a:as) :) <$> sequences cs)
    descending a as bs = ((a:as) :) <$> sequences bs

    ascending a as cs@(b:bs) =
      cmp a b >>=
      onGT ((as [a] :) <$> sequences cs) (ascending b (as . (a:)) bs)
    ascending a as bs = (as [a] :) <$> sequences bs

    mergeAll [x] = pure x
    mergeAll xs  = mergePairs xs >>= mergeAll

    mergePairs (a:b:xs) = (:) <$> merge a b <*> mergePairs xs
    mergePairs xs = pure xs

    merge as@(a:as') bs@(b:bs') =
      cmp a b >>= onGT ((b :) <$> merge as bs') ((a :) <$> merge as' bs)
    merge [] bs = pure bs
    merge as [] = pure as

    onGT gt ngt ord = if ord == GT then gt else ngt

--------------------------------------------------------------------------------
-- Transformers:
--------------------------------------------------------------------------------

-- | 'rebase': change base monad of from Identity to something else.
rebase :: (MFunctor t, Monad n) => t Identity b -> t n b
rebase = hoist generalize

-- | 'io': alias of 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Produces an ExceptT e m a, in any monad m, given an Either e a.
exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT e = rebase $ except e