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

-- | Functor, Applicative, Monad (+ transformer) utilities.
module Util.Monad (
  -- ** General utilities
    (<$$>)
  , unless'
  , fkeep
  , untilEqM
  , untilMatchM
  -- ** Monad stack transformations
  , rebase
  , io
  ) where

import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (MFunctor, hoist, generalize)

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
-- Transformers:
--------------------------------------------------------------------------------

-- | 'rebase': change base monad of from Identity to something else.
rebase :: (MFunctor t, Monad n) => t Identity b -> t n b
rebase = hoist generalize

-- | 'io': alias of 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO