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

{-# LANGUAGE StandaloneDeriving
  , GADTs, DataKinds, PolyKinds, LambdaCase #-}

-- | Types from Core.Start.AST as singletons.
module Core.TypeCheck.SType (
  -- * Data types:
    SPrimType (..)
  , SType (..)
  , TWrap (..)
  -- * Free utility functions:
  , dynPType
  , dynType
  , eqType
  , (=?=)
  ) where

import Data.Type.Equality ((:~:) (..), TestEquality, testEquality)
import Data.Proxy (Proxy (..))

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import Core.Start.AST

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

-- | Singleton versions of PrimType.
data SPrimType (t :: PrimType) where
  TBool   :: SPrimType BoolT   -- ^ Singleton type of bool   expressions.
  TByte   :: SPrimType ByteT   -- ^ Singleton type of byte   expressions.
  TShort  :: SPrimType ShortT  -- ^ Singleton type of short  expressions.
  TInt    :: SPrimType IntT    -- ^ Singleton type of int    expressions.
  TLong   :: SPrimType LongT   -- ^ Singleton type of long   expressions.
  TChar   :: SPrimType CharT   -- ^ Singleton type of char   expressions.
  TFloat  :: SPrimType FloatT  -- ^ Singleton type of float  expressions.
  TDouble :: SPrimType DoubleT -- ^ Singleton type of double expressions.

-- | Singleton versions of Type.
data SType (t :: Type) where
  TPrim  :: SPrimType t -> SType (PrimT t) -- ^ Singleton type of primitives.
  TStr   :: SType StringT -- ^ Singleton type of string expressions.
  TArray :: SType t -> SType (ArrayT t) -- Singleton types of arrays. Inductive.

-- | TWrap: Existentially quantified version of SType.
-- Exists to allow eqType conversion.
data TWrap where
  -- | Construct a TWrap given a proxy (witness for t) + the type.
  (::~) :: Proxy t -> SType t -> TWrap

deriving instance Eq       (SPrimType t)
deriving instance Ord      (SPrimType t)
deriving instance Show     (SPrimType t)
deriving instance Typeable (SPrimType t)
deriving instance Eq       (SType t)
deriving instance Show     (SType t)
deriving instance Typeable (SType t)

-- | dynPType: Convert (primitive type) singleton version to dynamic version.
dynPType :: SPrimType t -> PrimType
dynPType = \case
  TBool   -> BoolT
  TByte   -> ByteT
  TShort  -> ShortT
  TInt    -> IntT
  TLong   -> LongT
  TChar   -> CharT
  TFloat  -> FloatT
  TDouble -> DoubleT

-- | dynType :: Convert (type) singleton version to dynamic version.
dynType :: SType t -> Type
dynType = \case
  TPrim t  -> PrimT  $ dynPType t
  TStr     -> StringT
  TArray t -> ArrayT $ dynType t

-- | eqType :: convert dynamic Type to wrapper of
-- existentially quantified singleton version.
eqType :: Type -> TWrap
eqType = \case
  PrimT t   -> case t of
    BoolT   -> Proxy ::~ TPrim TBool
    ByteT   -> Proxy ::~ TPrim TByte
    ShortT  -> Proxy ::~ TPrim TShort
    IntT    -> Proxy ::~ TPrim TInt
    LongT   -> Proxy ::~ TPrim TLong
    CharT   -> Proxy ::~ TPrim TChar
    FloatT  -> Proxy ::~ TPrim TFloat
    DoubleT -> Proxy ::~ TPrim TDouble
  StringT   -> Proxy ::~ TStr
  ArrayT t  -> case eqType t of
    Proxy ::~ t' -> Proxy ::~ TArray t'

instance TestEquality SPrimType where
  testEquality TBool   TBool   = pure Refl
  testEquality TByte   TByte   = pure Refl
  testEquality TShort  TShort  = pure Refl
  testEquality TInt    TInt    = pure Refl
  testEquality TLong   TLong   = pure Refl
  testEquality TChar   TChar   = pure Refl
  testEquality TFloat  TFloat  = pure Refl
  testEquality TDouble TDouble = pure Refl
  testEquality _       _       = Nothing

instance TestEquality SType where
  testEquality (TPrim a)  (TPrim b)  = testEquality a b >>= \Refl -> pure Refl
  testEquality TStr       TStr       = pure Refl
  testEquality (TArray a) (TArray b) = testEquality a b >>= \Refl -> pure Refl
  testEquality _          _          = Nothing

-- | Determine if two types are the same at run time.
(=?=) :: TestEquality f => f a -> f b -> Maybe (a :~: b)
(=?=) = testEquality