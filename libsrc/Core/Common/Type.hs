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

-- | Module for Type:s and algebra for allowed conversions.
module Core.Common.Type where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Foldable (find, msum)
import Control.Monad (mfilter, mplus)
import Control.Lens (Lens', Traversal', _Just, (^?), isn't)

import Class.Sizeables

import Core.Common.TH
import Core.Common.Purity

--------------------------------------------------------------------------------
-- Data Types:
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
      _tPrim :: PrimType -- ^ A primitive type.
    }
  | StringT              -- ^ A String type.
  | ArrayT {
      _tType :: Type     -- ^ An array type of some other type.
    }
  | NullT                -- ^ Type of the null literal, can't be declared.
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

$(deriveLens [''PrimType, ''Type])

-- | Return types (including "void").
type RType = Maybe Type

--------------------------------------------------------------------------------
-- Array related:
--------------------------------------------------------------------------------

instance Growable   Type where
  grow = ArrayT

instance Shrinkable Type where
  shrink = \case ArrayT t -> t
                 x        -> x

-- | Dimensionality of a type, an array adds +1 dimensionality.
dimens :: Type -> Integer
dimens = \case
  PrimT _  -> 0
  StringT  -> 0
  ArrayT t -> 1 + dimens t
  NullT    -> 0

--------------------------------------------------------------------------------
-- Aliases:
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

-- | Yields True if the type is primitive numeric.
isTNum :: Type -> Bool
isTNum t = fromMaybe False $ isn't _BoolT <$> (t ^? tPrim)

-- | Yields True if the type is primitive integral.
isTInt :: Type -> Bool
isTInt t = t `elem` [byT, chT, shT, inT, loT]

--------------------------------------------------------------------------------
-- HasType:
--------------------------------------------------------------------------------

-- | Class of terms with knowledge about their type.
class HasType x where
  -- | Lens for the rtype of the term.
  rtype :: Lens' x RType

  -- | Traversal for the value type of the term.
  ttype :: Traversal' x Type
  ttype = rtype . _Just

--------------------------------------------------------------------------------
-- Type conversions:
--------------------------------------------------------------------------------

-- | Convert: PrimType -> PrimType,
-- if successful, the target type is in the result.
type PTConv = PrimType -> PrimType -> Maybe PrimType

-- | Convert: Type -> Type, if successful, the target type is in the result.
type TConv  = Type -> Type -> Maybe Type

-- | Pick the first conversion that is successful.
anyConv :: (Foldable t, Functor t) => t TConv -> TConv
anyConv convs s t = msum $ ($ s) . ($ t) <$> convs

-- | Lift conversion on primitive types into types.
liftPT :: PTConv -> TConv
liftPT c s t = s ^? tPrim >>= \sp -> t ^? tPrim >>= \tp -> PrimT <$> c sp tp

-- | Creates a conversion given a function from primitive type to the list
-- of primitive types it is convertible to.
ptAccept :: Foldable t => (PrimType -> t PrimType) -> TConv
ptAccept f = liftPT $ \s t -> find (== t) $ f s

-- | JLS 5.1.1, Identity conversion.
idConv :: TConv
idConv s t = if s == t then pure t else Nothing

-- | JLS 5.1.2, Widening primitive conversion.
wpConv :: TConv
wpConv = ptAccept $ \case
  ByteT  -> [ShortT, IntT, LongT, FloatT, DoubleT]
  ShortT -> [IntT, LongT, FloatT, DoubleT]
  CharT  -> [IntT, LongT, FloatT, DoubleT]
  IntT   -> [LongT, FloatT, DoubleT]
  LongT  -> [FloatT, DoubleT]
  FloatT -> [DoubleT]
  _      -> []

-- | JLS 5.1.3, Narrowing primitive conversion.
npConv :: TConv
npConv = ptAccept $ \case
  CharT   -> [ByteT, ShortT]
  ShortT  -> [ByteT, CharT]
  IntT    -> [ByteT, ShortT, CharT]
  LongT   -> [ByteT, ShortT, CharT, IntT]
  FloatT  -> [ByteT, ShortT, CharT, IntT, LongT]
  DoubleT -> [ByteT, ShortT, CharT, IntT, LongT, FloatT]
  _       -> []

-- | JLS 5.1.4, Widening + Narrowing primitive conversion.
wnpConv :: TConv
wnpConv = ptAccept $ \case
  ByteT -> [CharT]
  _     -> []

-- | JLS 5.1.5, Widening reference conversion.
wrConv :: TConv
wrConv s t = Nothing -- TODO: tier 1

-- | JLS 5.1.6, Narrowing reference conversion.
nrConv :: TConv
nrConv s t = Nothing -- TODO: tier 1

-- | JLS 5.1.7, Boxing conversion of primitive types.
boxConv :: TConv
boxConv s t = Nothing -- TODO: tier 1

-- | JLS 5.1.8, Unboxing conversion to primitive types.
unBoxConv :: TConv
unBoxConv s t = Nothing -- TODO: tier 1

-- | NullT ==OK==> T /= PrimT, NullT can be converted into any reference type.
nullConv :: TConv
nullConv s t = if s /= NullT then Nothing else case t of
    ArrayT _ -> pure t
    StringT  -> pure t
    _        -> Nothing

constNp2iConv :: Purity -> TConv
constNp2iConv = \case
  Constant -> ptAccept $ \case
    IntT   -> [ByteT, ShortT, CharT]
    _      -> []
  _        -> \_ _ -> Nothing

-- | JLS 5.2, narrowing primitive conversion of a constant.
constNpConv :: Purity -> TConv
constNpConv = \case
  Constant -> ptAccept $ \case
    ByteT  -> [CharT]
    CharT  -> [ByteT, ShortT]
    ShortT -> [ByteT, CharT]
    IntT   -> [ByteT, ShortT, CharT]
  _        -> \_ _ -> Nothing

-- | JLS 5.2, assignment context conversion.
assConv :: Purity -> TConv
assConv p = anyConv
          [ idConv, wpConv, wrConv, boxConv
          , unBoxConv, nullConv, constNpConv p]

-- | JLS 5.3, method application context conversion.
appConv :: TConv
appConv = anyConv [idConv, wpConv, boxConv, unBoxConv, nullConv]

{-
castConv :: TConv
castConv x y = coercesTo x y
-}

idUbConv :: TConv
idUbConv = anyConv [idConv, unBoxConv]

-- | Widening primitive conversion + Id conversion + Unboxing conversion.
wpIdConv :: TConv
wpIdConv = anyConv [idUbConv, wpConv]

-- | Applies conversion and further restricts it by matching against any
-- of the given types.
anyOf :: Foldable f => f Type -> TConv -> TConv
anyOf ok f s t = mfilter (`elem` ok) $ f s t

-- | A conversion in any direction.
binConv :: TConv -> TConv
binConv c = anyConv [c, flip c]

-- | A wpIdConv in any direction which is any of the types in the first arg.
binWpConv :: Foldable f => f Type -> TConv
binWpConv ok = anyOf ok $ binConv wpIdConv

-- | JLS 5.6.2, numeric binary promotion.
numBinConv :: TConv
numBinConv = binWpConv [doT, flT, loT, inT]

-- | Integer binary promotion.
intBinConv :: TConv
intBinConv = binWpConv [loT, inT]

-- | JLS 15.25., ternary operator.
condConv :: Purity -> Purity -> TConv
condConv pl pr = anyConv
           [ idConv, binConv unBoxConv, binConv nullConv
           , anyOf [shT] $ binConv wpConv
           , anyConv [constNp2iConv pl, flip (constNp2iConv pr)]
        -- , unBox...
           , numBinConv
           ]

-- | An identity conversion of given type.
eqConv :: Type -> TConv
eqConv ok s t = if ok == s && s == t then pure ok else Nothing

-- | BoolT => BoolT.
boolConv :: TConv
boolConv = convThen idUbConv (eqConv boT) boT

eqOpConv :: TConv
eqOpConv = anyConv [numBinConv, boolConv, idConv]

-- | Integer binary promotion or BoolT => BoolT.
biBinConv :: TConv
biBinConv = anyConv [intBinConv, boolConv]

-- | Conversion matching both given conversions.
bothConv :: TConv -> TConv -> TConv
bothConv a b s t = a s t >> b s t

-- | Use first conversion, then use second with the input of the first and type
-- given.
convThen :: TConv -> TConv -> Type -> TConv
convThen x y v s t = x s t >> y t v

type UConv = Type -> Maybe Type

unBin :: (Functor f, Foldable f) => f Type -> TConv -> UConv
unBin vs c s = msum $ c s <$> vs

-- | Pick the first conversion that is successful.
anyUConv :: (Functor f, Foldable f) => f UConv -> UConv
anyUConv convs s = msum $ ($ s) <$> convs

-- | JLS 5.6.1. Unary Numeric Promotion
unConv :: UConv
unConv = anyUConv
       [ unBin [inT] $ convThen unBoxConv (anyConv [idConv, wpConv]) inT
       , unBin [loT, flT, doT] unBoxConv
       , unBin [inT] wpConv
       , mfilter isTNum . pure
       ]

intUnConv :: UConv
intUnConv = mfilter isTInt . unConv

unConvSh :: TConv
unConvSh s t = intUnConv s <* intUnConv t