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

{-# LANGUAGE GADTs, TypeFamilies, DataKinds,
  TypeOperators, ConstraintKinds, StandaloneDeriving, LambdaCase #-}

module CoreAST where

import qualified Language.Java.Syntax as JS

import Data.Proxy (Proxy)
import Data.Sized (Sized)
import GHC.TypeLits (Nat)
import Data.Type.Natural ((:+))
import Data.Singletons.Prelude.Ord (Sing, (:>))
import Data.Singletons.Prelude.Enum (Pred)

type Min1   (n :: Nat)   = (n :> 0) ~ True
type Vector (n :: *)   a = Sized [] n a
type SVec   (n :: Nat) a = Vector (Sing n) a





class PhaseIndex p where
  mkPhase   :: p
  phaseName :: p -> String

data PDynamic = PDynamic
  deriving (Eq, Ord, Enum, Show, Read)

instance PhaseIndex PDynamic where
  mkPhase     = PDynamic
  phaseName _ = "Initial, Dynamic Phase"

class IsBool   t where
class CanAdd   t where
class CanSub   t where
class CanMul   t where
class CanCmp   t where
class CanStep  t where
class CanShift t where
class CanBitM  t where

data NumOp t where
  Add     :: CanAdd   t => NumOp t
  Sub     :: CanSub   t => NumOp t
  Mul     :: CanMul   t => NumOp t
  Div     :: CanMul   t => NumOp t
  Rem     :: CanMul   t => NumOp t
  LShift  :: CanShift t => NumOp t
  RShift  :: CanShift t => NumOp t
  RRShift :: CanShift t => NumOp t
  And     :: CanBitM  t => NumOp t
  Xor     :: CanBitM  t => NumOp t
  Or      :: CanBitM  t => NumOp t

deriving instance Eq t => Eq  (NumOp t)
deriving instance Eq t => Ord (NumOp t)
deriving instance Show (NumOp t)


data EquOp = EQ | NE
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data CmpOp = LT | GT | LE | GE
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data LogOp = LAnd | LOr
  deriving (Eq, Ord, Enum, Bounded, Show, Read)



type family XLVName p t
type family XLVArray p t

type family ArrIdxT p
type IExpr p a = Expr p (ArrIdxT p) a
type IxEVec (n :: Nat) p a = SVec n (IExpr p a)

data LValue p t a where
  LVName  :: XLVName  p t -> JS.Name -> LValue p t a
  LVArray :: Min1 n => XLVArray p t -> Expr p t a -> IxEVec n p a -> LValue p t a





type family XLit     p t
type family ELit     p t
type family XVar     p t
type family XCast    p t tf
type family XCond    p t
type family XAssign  p t
type family XOAssign p t
type family XEqu     p t
type family XNEqu    p t
type family XNum     p t
type family XLT      p t
type family XGT      p t
type family XLE      p t
type family XGE      p t
type family XAnd     p t
type family XOr      p t
type family XNot     p t
type family XPostInc p t
type family XPostDec p t
type family XPreInc  p t
type family XPreDec  p t
type family XMinus   p t
type family XBCompl  p t
type family XMApp    p t
type family XMAppArg p a
type family EArrNewT p t
type family XArrNew  p t

-- data TypedExpr p a where
--  (:::) :: Type t -> Expr p t a -> TypedExpr p a

type family PromoteToArr p t (dims :: Nat)
type family EANInit      p t (dims :: Nat) a

type AExpr p t s a = Expr p (PromoteToArr p t s) a

data Expr p t a where
  ELit  :: a -> XLit p t -> ELit p t -> Expr p t a
  EVar  :: a -> XVar p t -> LValue p t a -> Expr p t a
  ECast :: a -> XCast p t tf -> Expr p tf a -> Expr p t a
  ECond :: IsBool tb => a -> XCond p t -> Expr p tb a -> Expr p t a -> Expr p t a -> Expr p t a
  EAssign  :: a -> XAssign p t  -> LValue p t a -> Expr p t a -> Expr p t a
  EOAssign :: a -> XOAssign p t -> LValue p t a -> NumOp t -> Expr p t a -> Expr p t a
  ENum :: a -> XNum p t -> Expr p t a -> Expr p t a -> NumOp t -> Expr p t a
  EEqu :: IsBool tb => a -> XEqu p t  -> Expr p t a -> Expr p t a -> Expr p tb a
  ENEq :: IsBool tb => a -> XNEqu p t -> Expr p t a -> Expr p t a -> Expr p tb a
  ELT :: (IsBool tb, CanCmp t) => a -> XLT p t -> Expr p t a -> Expr p t a -> Expr p tb a
  EGT :: (IsBool tb, CanCmp t) => a -> XGT p t -> Expr p t a -> Expr p t a -> Expr p tb a
  ELE :: (IsBool tb, CanCmp t) => a -> XLE p t -> Expr p t a -> Expr p t a -> Expr p tb a
  EGE :: (IsBool tb, CanCmp t) => a -> XGE p t -> Expr p t a -> Expr p t a -> Expr p tb a
  EAnd :: IsBool t => a -> XAnd p t -> Expr p t a -> Expr p t a -> Expr p t a
  EOr  :: IsBool t => a -> XOr  p t -> Expr p t a -> Expr p t a -> Expr p t a
  ENot :: IsBool t => a -> XNot p t -> Expr p t a -> Expr p t a
  EPostInc :: CanStep t => a -> XPostInc p t -> Expr p t a -> Expr p t a
  EPostDec :: CanStep t => a -> XPostDec p t -> Expr p t a -> Expr p t a
  EPreInc  :: CanStep t => a -> XPreInc  p t -> Expr p t a -> Expr p t a
  EPreDec  :: CanStep t => a -> XPreDec  p t -> Expr p t a -> Expr p t a
  EBCompl :: CanBitM t => a -> XBCompl p t -> Expr p t a -> Expr p t a
  EMinus :: CanSub t => a -> XMinus p t -> Expr p t a -> Expr p t a
  EMApp :: a -> XMApp p t -> JS.Name -> [XMAppArg p a] -> Expr p t a  
  EArrNew :: Min1 s1 => a -> XArrNew p t -> EArrNewT p t
          -> Proxy t -> IxEVec s1 p a
          -> Sing s2 -> AExpr p t (s1 :+ s2) a
  EArrNewInit :: a -> XArrNew p t -> EArrNewT p t -> Sing s ->
          Proxy t -> EANInit p t s a -> AExpr p t s a

data VarInit s p t a where
  InitExpr :: Expr p t a -> VarInit 0 p t a
  InitArr  :: Min1 s => ArrayInit s p t a -> VarInit s p t a

data ArrayInit n p t a = ArrayInit [VarInit (Pred n) p t a]


type instance ELit PDynamic t = JS.Literal
type instance XCast PDynamic t2 t1 = JS.Type