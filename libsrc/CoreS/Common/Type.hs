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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, LambdaCase #-}

-- | Type related parts of AST + utilites.
module CoreS.Common.Type where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.Maybe (fromMaybe)
import Control.Lens ((^?), isn't)

import Class.Sizeables (Growable, grow, Shrinkable, shrink)
import Util.TH (deriveLens)

import CoreS.Common.Name (Name, VarDeclId)

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

-- | Primitive types.
data PrimType
  = BoolT   -- ^ Type of bool   values and expressions.
  | ByteT   -- ^ Type of byte   values and expressions.
  | ShortT  -- ^ Type of short  values and expressions.
  | IntT    -- ^ Type of int    values and expressions.
  | LongT   -- ^ Type of long   values and expressions.
  | CharT   -- ^ Type of char   values and expressions.
  | FloatT  -- ^ Type of float  values and expressions.
  | DoubleT -- ^ Type of double values and expressions.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | All possible types that value / expression can be of.
data Type
  = PrimT {
      _tPrim  :: PrimType -- ^ A primitive type.
    }
  | StringT               -- ^ A String type.
  | NullT                 -- ^ Type of the null literal, can't be declared.
  | ClassType {
      _tClass :: Name     -- ^ A user defined or built-in class type.
    }
  | ArrayT {
      _tType  :: Type     -- ^ An array type of some other type.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Return types (including "void").
type RType = Maybe Type

--------------------------------------------------------------------------------
-- Type modifiers:
--------------------------------------------------------------------------------

-- | VarMod: A variable can either be declared as final, or not.
data VarMod = VMFinal | VMNormal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | VMType: VarMod + Type.
data VMType
  = VMType {
      _vmMod  :: VarMod -- ^ Modifier of type (final / not).
    , _vmType :: Type   -- ^ Base type of the VMType.
    }
  | HoleVMType {
      _vmHole :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Formal parameters (typed argument to a function):
--------------------------------------------------------------------------------

-- | FormalParam: formal parameter of a method.
data FormalParam = FormalParam
  { _fpType :: VMType    -- ^ Type of parameter.
  , _fpVDI  :: VarDeclId -- ^ Identifier of parameter.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- NFData:
--------------------------------------------------------------------------------

instance NFData PrimType
instance NFData Type
instance NFData VarMod
instance NFData VMType
instance NFData FormalParam

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [''PrimType, ''Type, ''VarMod, ''VMType, ''FormalParam])

--------------------------------------------------------------------------------
-- Types, Aliases:
--------------------------------------------------------------------------------

-- | Short hand for PrimT BoolT.
boT :: Type
boT = PrimT BoolT

-- | Short hand for PrimT ByteT.
byT :: Type
byT = PrimT ByteT

-- | Short hand for PrimT CharT.
chT :: Type
chT = PrimT CharT

-- | Short hand for PrimT ShortT.
shT :: Type
shT = PrimT ShortT

-- | Short hand for PrimT IntT.
inT :: Type
inT = PrimT IntT

-- | Short hand for PrimT LongT.
loT :: Type
loT = PrimT LongT

-- | Short hand for PrimT FloatT.
flT :: Type
flT = PrimT FloatT

-- | Short hand for PrimT DoubleT.
doT :: Type
doT = PrimT DoubleT

--------------------------------------------------------------------------------
-- Sizability of types:
--------------------------------------------------------------------------------

instance Growable   Type where
  grow = ArrayT

instance Shrinkable Type where
  shrink = \case ArrayT t -> t
                 x        -> x

--------------------------------------------------------------------------------
-- Utilities:
--------------------------------------------------------------------------------

-- | Yields True if the type is primitive numeric.
isTNum :: Type -> Bool
isTNum t = fromMaybe False $ isn't _BoolT <$> (t ^? tPrim)

-- | Yields True if the type is primitive integral.
isTInt :: Type -> Bool
isTInt = (`elem` [byT, chT, shT, inT, loT])

-- Fold a type into something else recursively until it reaches a base type.
-- Tail recursive fold.
typeFold :: (b -> Type -> b) -> b -> Type -> b
typeFold f z = \case
  ArrayT t -> typeFold f (f z t) t
  t        -> z

-- | Dimensionality of a type, an array adds +1 dimensionality.
typeDimens :: Type -> Integer
typeDimens = typeFold (const . (+1)) 0

-- | Base type of type - given a base type, this is id.
typeBase :: Type -> Type
typeBase t = typeFold (flip const) t t