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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving
  , TemplateHaskell, TupleSections, StandaloneDeriving
  , MultiParamTypeClasses, KindSignatures #-}

-- | Normalizer monad and utilities.
module Norm.NormM (
    module Norm.NormM
  , module RE
  ) where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Monoid ((<>), Any (..))
import Control.Arrow ((&&&))
import Control.Monad (ap)
import Control.Applicative (Alternative)

-- Re-Exports:
import Control.Comonad            as RE
import Control.Monad.Identity     as RE
import Control.Monad.Writer.Class as RE
import Control.Monad.Fix          as RE
import Control.Monad.IO.Class     as RE

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Zip (MonadZip)
import Control.Monad.Writer (WriterT (..), runWriterT)
import Control.Lens (transformMOf, transformMOnOf, traverseOf)
import Data.Data.Lens (uniplate, biplate)

import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary)

import Util.TH (deriveLens)

u = undefined

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

-- | Convert from Bool. True ==> Change. Preserves monoidal properties.
fromBool :: Bool -> Unique
fromBool u = if u then Change else Unique

-- | Convert from Any. See fromBool.
fromAny :: Any -> Unique
fromAny = fromBool . getAny

instance Arbitrary Unique where
  arbitrary = fromBool <$> arbitrary

--------------------------------------------------------------------------------
-- Norm-alizer monad:
--------------------------------------------------------------------------------

-- | Norm: The normalizer monad.
-- This provides an efficient and ergonomic interface to writing normalizers.
--
-- Isomorphic to (Any, a).
-- The kleisli arrow (a -> Norm a) is isomorphic to (a -> Maybe a).
-- Eq a => (a -> Norm a) is also isomorphic to (a -> a).
newtype NormT m (a :: *) = NormT { _runNormT :: WriterT Unique m a }
  deriving ( Eq, Ord, Show, Read, Generic, Typeable -- TOOD: , Data
           , Functor, Applicative, Monad, MonadFix, MonadIO, MonadZip
           , Alternative, MonadPlus, MonadTrans
           , MonadWriter Unique )

-- | Standard normalizer, using 'Identity' as base monad.
type Norm = NormT Identity

--------------------------------------------------------------------------------
-- Kleisli arrows:
--------------------------------------------------------------------------------

-- | NormArrT: kleisli arrow for NormT.
type NormArrT m a = a -> NormT m a

-- | NormArr: kleisli arrow for Norm.
type NormArr a = a -> Norm a

--------------------------------------------------------------------------------
-- Runners:
--------------------------------------------------------------------------------

-- | Run the normalizing computation yielding the base monad containing a pair
-- consisting of the resulting term and the uniqueness.
runNT :: NormT m a -> m (a, Unique)
runNT = runWriterT . _runNormT

-- | Run the normalizing computation yielding the base monad containing
-- the resulting term.
runTerm :: Functor m => NormT m a -> m a
runTerm = fmap fst . runNT

-- | Run the normalizing computation yielding the base monad containing
-- the uniqueness.
execUnique :: Functor m => NormT m a -> m Unique
execUnique = fmap snd . runNT

-- | Run the normalizing computation yielding a pair consisting
-- of the resulting term and the uniqueness.
-- Only works for Comonad m.
extNT :: Comonad m => NormT m a -> (a, Unique)
extNT = extract . runNT

-- | Run the normalizing computation yielding the resulting term.
-- Only works for Comonad m.
extTerm :: Comonad m => NormT m a -> a
extTerm = extract . runTerm

-- | Run the normalizing computation yielding the uniqueness.
-- Only works for Comonad m.
extUnique :: Comonad m => NormT m a -> Unique
extUnique = extract . execUnique

--------------------------------------------------------------------------------
-- Construction:
--------------------------------------------------------------------------------

-- | A normalization that was already unique (no change).
-- Equivalent to pure.
unique :: Applicative m => a -> NormT m a
unique = pure

-- | A normalization that changed something.
change :: Monad m => a -> NormT m a
change = normMake . (,Change)

normMake :: Applicative m => (a, Unique) -> NormT m a
normMake (a, w) = NormT $ WriterT $ pure (a, w)

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance (Comonad m, Applicative m) => Comonad (NormT m) where
  extract   = extTerm
  duplicate = normMake . (id &&& extUnique)

-- | Match on NormT.
normT :: Monad m => (Unique -> a -> m b) -> NormT m a -> m b
normT f n = runNT n >>= \(a, w) -> f w a

-- | Match on NormT.
norm :: Comonad m => (Unique -> a -> b) -> NormT m a -> b
norm f = uncurry (flip f) . extNT

-- | Runs a normalizer on a term until it is in unique normal form.
normLoop :: Monad m => NormArrT m a -> a -> m a
normLoop f = normT (\u -> if isUnique u then pure else normLoop f) . f

instance (Monad m, Arbitrary a) => Arbitrary (NormT m a) where
  arbitrary = NormT . WriterT . pure <$> ((,) <$> arbitrary <*> arbitrary)

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
convNEq = (extTerm .)

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
normEveryT :: (Monad m, Data a) => (a -> NormT m a) -> a -> NormT m a
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
normImm :: (Monad m, Data a) => (a -> NormT m a) -> a -> NormT m a
normImm = traverseOf uniplate

--------------------------------------------------------------------------------
-- Lens:
--------------------------------------------------------------------------------

$(deriveLens [''Unique, ''NormT])