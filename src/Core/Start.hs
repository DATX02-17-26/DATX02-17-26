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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies, DataKinds, LambdaCase, TupleSections,
  ScopedTypeVariables, TypeOperators, TypeApplications #-}

module Core.Start where

import Safe (headMay)

import Data.Proxy (Proxy (..))
import Data.Void
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import GHC.TypeLits (Nat)
import Data.Sized (fromList', fromList, singleton, toSomeSized, SomeSized)
import Data.Singletons (fromSing, toSing, Sing, SomeSing (SomeSing))
import Data.Singletons.TypeLits (SNat, Sing (..))
import Data.Singletons.Prelude.Bool (Sing (..))
import Data.Singletons.Prelude.Ord ((:>), (%:>))
import Data.Singletons.Prelude.Num ((%:+))
import Data.Type.Equality (testEquality, (:~:) (..))

import qualified Language.Java.Syntax as S

import Core.AST
import Core.SVec


data Start = Start
  deriving (Eq, Ord, Enum, Show, Read)

instance PhaseIndex Start where
  mkPhase     = Start
  phaseName _ = "Start, Dynamic Phase"

u = undefined

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

data PrimType
  = BoolT
  | ByteT
  | ShortT
  | IntT
  | LongT
  | CharT
  | FloatT
  | DoubleT
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

data Type
  = PrimType PrimType
  | StringT
  | ArrayT Type
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data RefType
  = CRType Ident
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)


instance CanAdd   Type where
instance CanSub   Type where
instance CanMul   Type where
instance CanCmp   Type where
instance CanStep  Type where
instance CanShift Type where
instance CanBitM  Type where

--------------------------------------------------------------------------------
-- Expressions:
--------------------------------------------------------------------------------

-- | A literal denotes a fixed, unchanging value.
data Literal
  = Int Integer
  | Word Integer
  | Float Double
  | Double Double
  | Boolean Bool
  | Char Char
  | String String
  | Null
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

type instance XArrIdxT Start     = Type
type instance XBoolT   Start     = Type
type instance XToArr _ Start _   = Type
type instance XDefT    Start _   = Type
type instance XTEType  Start _   = Void
type instance XLitT    Start _   = Literal

type instance XLVName  Start _   = Void
type instance XLVArray Start _   = Void
type instance XExpr    Start _   = Void
type instance XLit     Start _   = Void
type instance XVar     Start _   = Void
type instance XCast    Start _ _ = Void
type instance XCond    Start _   = Void
type instance XAssign  Start _   = Void
type instance XOAssign Start _   = Void
type instance XEqu     Start _   = Void
type instance XNEqu    Start _   = Void
type instance XNum     Start _   = Void
type instance XLT      Start _   = Void
type instance XGT      Start _   = Void
type instance XLE      Start _   = Void
type instance XGE      Start _   = Void
type instance XAnd     Start     = Void
type instance XOr      Start     = Void
type instance XNot     Start     = Void
type instance XPostInc Start _   = Void
type instance XPostDec Start _   = Void
type instance XPreInc  Start _   = Void
type instance XPreDec  Start _   = Void
type instance XMinus   Start _   = Void
type instance XBCompl  Start _   = Void
type instance XMApp    Start _   = Void
type instance XMAppArg Start _   = Void
type instance XArrNew  Start _   = Void

--------------------------------------------------------------------------------
-- Conversion:
--------------------------------------------------------------------------------

type SExpr   = Expr Start Type ()
type ConvErr = String
type CConv a = Either ConvErr a

void :: Void
void = error "Attempt to evaluate void"

unimpl :: Show x => x -> CConv y
unimpl x = Left $ show x ++ " is not supported yet!"

st :: (() -> Void -> t) -> t
st ctor = ctor () void

--------------------------------------------------------------------------------
-- Conversion, Types:
--------------------------------------------------------------------------------

convPTyp :: S.PrimType -> PrimType
convPTyp = \case
  S.BooleanT -> BoolT
  S.ByteT    -> ByteT
  S.ShortT   -> ShortT
  S.IntT     -> IntT
  S.LongT    -> LongT
  S.CharT    -> CharT
  S.FloatT   -> FloatT
  S.DoubleT  -> DoubleT

strTPrefix = S.ClassType $ ((, []) . S.Ident) <$> ["java", "lang", "String"]
strT       = S.ClassType $ ((, []) . S.Ident) <$> ["String"]

convRTyp :: S.RefType -> CConv Type
convRTyp = \case
  S.ClassRefType ct |
    ct `elem` [strT, strTPrefix] -> pure StringT
  S.ArrayType    t               -> ArrayT <$> convTyp t
  x                              -> unimpl x

convTyp :: S.Type -> CConv Type
convTyp = \case
  S.PrimType t -> pure $ PrimType $ convPTyp t
  S.RefType  t -> convRTyp t

--------------------------------------------------------------------------------
-- Conversion, Expression:
--------------------------------------------------------------------------------

convLit :: S.Literal -> CConv SExpr
convLit = pure . (st ELit) . \case
  S.Int     i -> Int     i
  S.Word    w -> Word    w
  S.Float   f -> Float   f
  S.Double  d -> Double  d
  S.Boolean b -> Boolean b
  S.Char    c -> Char    c
  S.String  s -> String  s
  S.Null      -> Null

convUna :: S.Exp -> (() -> Void -> SExpr -> y) -> CConv y
convUna e ctor = st ctor <$> convExp e

convBin :: S.Exp -> S.Exp -> (() -> Void -> SExpr -> SExpr -> y) -> CConv y
convBin l r ctor = st ctor <$> convExp l <*> convExp r

convNum :: S.Exp -> S.Exp -> (NumOp Type) -> CConv SExpr
convNum l r op = (st ENum) op <$> convExp l <*> convExp r

convBOp :: S.Exp -> S.Exp -> S.Op -> CConv SExpr
convBOp l r = \case
  S.Add     -> convNum l r Add
  S.Sub     -> convNum l r Sub
  S.Mult    -> convNum l r Mul 
  S.Div     -> convNum l r Div
  S.Rem     -> convNum l r Rem  
  S.And     -> convNum l r And
  S.Xor     -> convNum l r Xor
  S.Or      -> convNum l r Or
  S.LShift  -> convNum l r LShift
  S.RShift  -> convNum l r RShift
  S.RRShift -> convNum l r RRShift
  S.LThan   -> convBin l r ELT
  S.GThan   -> convBin l r EGT
  S.LThanE  -> convBin l r ELE
  S.GThanE  -> convBin l r EGE
  S.Equal   -> convBin l r EEqu
  S.NotEq   -> convBin l r ENEq
  S.CAnd    -> convBin l r EAnd
  S.COr     -> convBin l r EOr

convId :: S.Ident -> Ident
convId (S.Ident i) = Ident i

convExpN :: S.Name -> CConv SExpr
convExpN n@(S.Name ns) = (st EVar) . LVName void . convId
                     <$> maybe (unimpl n) pure (headMay ns)

convArrAcc :: S.ArrayIndex -> CConv SExpr
convArrAcc (S.ArrayIndex e eis) = do
  e'   <- convExp e
  eis2 <- mapM convExp eis >>= maybe (unimpl eis) pure . toM1Vec
  pure $ (st EVar) $ LVArray void e' eis2

convArrCreate :: S.Type -> [S.Exp] -> Int -> CConv SExpr
convArrCreate t ls lex = do
  t'   <- convTyp t
  ls'  <- mapM convExp ls
  r    <- case (slen ls', toSing $ toInteger lex) of
    (SomeSing n, SomeSing m) -> do
      Refl <- minOneE (unimpl lex) (n %:+ m)
      ls'' <- maybe (unimpl ls) pure (fromList n ls')
      pure $ (st EArrNew) (Proxy, t') n ls'' m
  pure r

convExp :: S.Exp -> CConv SExpr
convExp = \case
  S.Lit                 lit -> convLit lit
  S.ArrayCreate    t ls lex -> convArrCreate t ls lex
  S.ArrayCreateInit t dl ai -> u
  S.MethodInv    mi         -> u
  S.ArrayAccess  ai         -> convArrAcc ai
  S.ExpName       n         -> convExpN n
  S.PostIncrement e         -> convUna e EPostInc
  S.PostDecrement e         -> convUna e EPostDec
  S.PreIncrement  e         -> convUna e EPreInc
  S.PreDecrement  e         -> convUna e EPreDec
  S.PrePlus       e         -> u
  S.PreMinus      e         -> convUna e EMinus
  S.PreBitCompl   e         -> convUna e EBCompl
  S.PreNot        e         -> convUna e ENot
  S.Cast        t e         -> st ECast <$> convTyp t <*> convExp e
  S.BinOp     l o r         -> convBOp l r o
  S.Cond      c i e         -> st ECond <$> convExp c <*> convExp i
                                                      <*> convExp e
  S.Assign   lv o e         -> u
  x                         -> unimpl x