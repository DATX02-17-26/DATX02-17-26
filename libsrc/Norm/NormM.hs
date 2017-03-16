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
  , TemplateHaskell, MultiParamTypeClasses, TupleSections #-}

-- | Normalizer monad and utilities.
module Norm.NormM where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Monoid ((<>), Any (..))
import Control.Monad (ap)
import Control.Comonad (Comonad, extract, duplicate)
import Control.Monad.Writer.Class (MonadWriter, writer, tell, listen, pass)

import Control.Lens (transformMOf, transformMOnOf, traverseOf)
import Data.Data.Lens (uniplate, biplate)

import Util.TH (deriveLens)

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

-- | Match on Norm.
norm :: (Unique -> a -> b) -> Norm a -> b
norm f (Norm u a) = f u a

instance Applicative Norm where
  pure  = unique
  (<*>) = ap

instance Monad Norm where
  return         = unique
  Norm u a >>= f = let Norm v b = f a in Norm (u <> v) b

instance Comonad Norm where
  extract = _normResult
  duplicate n = Norm (_normUnique n) n

instance MonadWriter Unique Norm where
  writer = uncurry $ flip Norm
  tell   = writer . ((),)
  listen = norm $ \u a      -> Norm u (a, u)
  pass   = norm $ \u (a, f) -> Norm (f u) a

--------------------------------------------------------------------------------
-- Isomorphisms:
--------------------------------------------------------------------------------

convMayN :: (a -> Maybe a) -> a -> Norm a
convMayN f a = maybe (unique a) change (f a)

convEqN :: Eq a => (a -> a) -> a -> Norm a
convEqN f a = let a' = f a in (if a == a' then unique else change) a'

convNMay :: (a -> Norm a) -> a -> Maybe a
convNMay f a = norm (\u -> if isChange u then Just else const Nothing) (f a)

convNEq :: (a -> Norm a) -> a -> a
convNEq f = _normResult . f

convMayEq :: (a -> Maybe a) -> a -> a
convMayEq = convNEq . convMayN

convEqMay :: Eq a => (a -> a) -> a -> Maybe a
convEqMay = convNMay . convEqN

--------------------------------------------------------------------------------
-- "Uniplate":
--------------------------------------------------------------------------------

-- | Recursively transforms all self similar decendants.
-- NOTE: this will pass type boundaries.
--
-- Imagine a toy language:
-- @
--   data B = B E
--     deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
--   
--   data E = V String | I Int | E :+: E | E :*: E | EB B
--     deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
--   
--   infixl 7 :*:
--   infixl 6 :+:
--   
--   instance Plated B where plate = uniplate
--   instance Plated E where plate = uniplate
--   
--   n1 = \e -> case e of
--     V "x" :+: V "x" -> change $ I 2 :*: V "x"
--     x               -> unique x
-- @
--
-- In this language,
-- > normEveryT n1 $ (EB $ B $ (V "x" :+: V "x")) :*: I 3
-- yields:
-- > Norm Change $ EB (B (I 2 :*: V "x")) :*: I 3
normEveryT :: Data a => (a -> Norm a) -> a -> Norm a
normEveryT = transformMOf uniplate

-- | Recursively transforms all self similar decendants of type a in a given s,
-- crossing type boundaries while doing so.
--
-- For the language above,
-- > normEvery n1 $ B $ (EB $ B $ (V "x" :+: V "x")) :*: I 3
-- yields:
-- > Norm Change $ B (EB (B (I 2 :*: V "x")) :*: I 3)
normEvery :: (Data s, Typeable a, Data a) => (a -> Norm a) -> s -> Norm s
normEvery = transformMOnOf biplate uniplate

-- | Recursively transforms all self similar immediate children of type a,
-- crossing type boundaries while doing so.
--
-- For the language above,
-- > normImm n1 $ (V "x" :+: V "x") :*: (I 1 :+: (V "x" :+: V "x"))
-- yields:
-- > Norm Change $ (I 2 :*: V "x") :*: (I 1 :+: (V "x" :+: V "x"))
normImm :: (Data a) => (a -> Norm a) -> a -> Norm a
normImm = traverseOf uniplate

--------------------------------------------------------------------------------
-- Lens:
--------------------------------------------------------------------------------

$(deriveLens [''Unique, ''Norm])