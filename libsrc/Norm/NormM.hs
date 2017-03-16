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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFunctor
  , TemplateHaskell #-}

-- | Normalizer monad and utilities.
module Norm.NormM where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Monoid ((<>), Any (..))
import Control.Monad (ap)
import Control.Comonad (Comonad, extract, duplicate)

import Util.TH

--------------------------------------------------------------------------------
-- Unique(ness):
--------------------------------------------------------------------------------

-- | Unique: was a term normalized or was it already in normal form according
-- to some specific normalizer? Isomorphic to Any (and transitively to Bool).
data Unique
  = Unique -- ^ Denotes that it already was in normal form.
  | Change -- ^ Denotes that it wasn't.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

instance Monoid Unique where
  mempty      = Unique
  mappend x y = if x < y then y else x

-- | Was the term already unique?
isUnique :: Unique -> Bool
isUnique = (== Unique)

-- | Did the term change?
isChange :: Unique -> Bool
isChange = (== Change)

-- | Convert to Any. It is "true" with isChange semantics.
toAny :: Unique -> Any
toAny = Any . isChange

--------------------------------------------------------------------------------
-- Norm-alizer monad:
--------------------------------------------------------------------------------

-- | Norm: The normalizer monad.
-- This provides an efficient and ergonomic interface to writing normalizers.
--
-- Isomorphic to (Any, a).
-- The kleisli arrow (a -> Norm a) is isomorphic to (a -> Maybe a).
-- Eq a => (a -> Norm a) is also isomorphic to (a -> a).
data Norm a = Norm
  { _normUnique :: Unique  -- ^ Was the computation normalizing?
  , _normResult :: a       -- ^ The result of the computation.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Functor)

-- | A normalization that was already unique (no change).
-- Equivalent to pure.
unique :: a -> Norm a
unique = Norm Unique

-- | A normalization that changed something.
change :: a -> Norm a
change = Norm Change

instance Applicative Norm where
  pure  = unique
  (<*>) = ap

instance Monad Norm where
  return         = unique
  Norm u a >>= f = let Norm v b = f a in Norm (u <> v) b

instance Comonad Norm where
  extract = _normResult
  duplicate n = Norm (_normUnique n) n

-- | Match on Norm.
norm :: Norm a -> (Unique -> a -> b) -> b
norm (Norm u a) f = f u a

--------------------------------------------------------------------------------
-- Isomorphisms:
--------------------------------------------------------------------------------

convMayN :: (a -> Maybe a) -> a -> Norm a
convMayN f a = maybe (unique a) change (f a)

convEqN :: Eq a => (a -> a) -> a -> Norm a
convEqN f a = let a' = f a in (if a == a' then unique else change) a'

convNMay :: (a -> Norm a) -> a -> Maybe a
convNMay f a = norm (f a) $ \u -> if isChange u then Just else const Nothing

convNEq :: (a -> Norm a) -> a -> a
convNEq f = _normResult . f

convMayEq :: (a -> Maybe a) -> a -> a
convMayEq = convNEq . convMayN

convEqMay :: Eq a => (a -> a) -> a -> Maybe a
convEqMay = convNMay . convEqN

--------------------------------------------------------------------------------
-- Lens:
--------------------------------------------------------------------------------

$(deriveLens [''Unique, ''Norm])