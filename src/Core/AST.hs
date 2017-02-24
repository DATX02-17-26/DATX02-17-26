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

{-# LANGUAGE GADTs, TypeFamilies, TypeInType,
  TypeOperators, ConstraintKinds, StandaloneDeriving, LambdaCase #-}

module Core.AST where

import Data.Void
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import Data.Proxy (Proxy)
import Data.Singletons.TypeLits (Nat, SNat)
import Data.Singletons.Prelude.Num ((:+))
import Data.Singletons.Prelude.Enum (Pred)

import Core.SVec

--------------------------------------------------------------------------------
-- Names and identifiers:
--------------------------------------------------------------------------------

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data Name = Name [Ident]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Phase Indexing:
--------------------------------------------------------------------------------

class PhaseIndex p where
  mkPhase   :: p
  phaseName :: p -> String

--------------------------------------------------------------------------------
-- Classes for Type:
--------------------------------------------------------------------------------

class CanAdd   t where
class CanSub   t where
class CanMul   t where
class CanCmp   t where
class CanStep  t where
class CanShift t where
class CanBitM  t where

--------------------------------------------------------------------------------
-- Operations on Numeric Type(s):
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Expressions:
--------------------------------------------------------------------------------


type family XLVName p t
type family XLVArray p t

type family XArrIdxT p
type IExpr p a = Expr p (XArrIdxT p) a
type IxEVec (n :: Nat) p a = SVec n (IExpr p a)


data LValue p t a where
  LVName  :: XLVName  p t -> Ident -> LValue p t a
  LVArray :: XLVArray p t -> Expr p t a -> M1Vec (IExpr p a) -> LValue p t a




type family XExpr    p t
type family XLit     p t
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
type family XAnd     p
type family XOr      p
type family XNot     p
type family XPostInc p t
type family XPostDec p t
type family XPreInc  p t
type family XPreDec  p t
type family XMinus   p t
type family XBCompl  p t
type family XMApp    p t
type family XMAppArg p a
type family XArrNew  p t
type family XSysOut  p
type family XSysErr  p

type family XLitT    p t
type family XBoolT   p
type family XVoidT   p
type family XToArr   (dims :: Nat) p t
type family XVarInit (dims :: Nat) p t a

data VarInit (s :: Nat) p t a where
  InitExpr :: Expr p t a -> VarInit 0 p t a
  InitArr  :: Min1 s => ArrayInit s p t a -> VarInit s p t a

data ArrayInit (s :: Nat) p t a = ArrayInit [XVarInit (Pred s) p t a]

type AExpr s p t a = Expr p (XToArr s p t) a
type BExpr p a = Expr p (XBoolT p) a
type VExpr p a = Expr p (XVoidT p) a

type family XDefT   p t
type family XTEType p t -- TODO REMOVE!
type family XTypedExpr p a
type XDefType p t = (Proxy t, XDefT p t)

data TypedExpr p a where
  (:::) :: XTEType p t -> Expr p t a -> TypedExpr p a

data Expr p t a where
  EExpr :: {
      _eAnot  :: a
    , _eXExpr :: XExpr p t
    } -> Expr p t a
  ELit  :: {
      _eAnot :: a
    , _eXLit :: XLit p t
    , _eLit  :: XLitT p t
    } -> Expr p t a
  EVar  :: {
      _eAnot   :: a
    , _eXVar   :: XVar p t
    , _eLValue :: LValue p t a
    } -> Expr p t a
  ECast :: {
      _eAnot  :: a
    , _eXCast :: XCast p t tf
    , _eCastT :: XDefT p tf 
    , _eCExpr :: Expr p tf a
    } -> Expr p t a
  ECond :: {
      _eAnot  :: a
    , _eXCond :: XCond p t
    , _eCond  :: BExpr p a
    , _eEi    :: Expr p t a
    , _eEe    :: Expr p t a
    } -> Expr p t a
  EAssign  :: {
      _eAnot    :: a
    , _eXAssign :: XAssign p t
    , _eLValue  :: LValue p t a
    , _eExpr    :: Expr p t a
    } -> Expr p t a
  EOAssign :: {
      _eAnot :: a
    , _eXOAssign :: XOAssign p t
    , _eLValue  :: LValue p t a
    , _eNumOp   :: NumOp t
    , _eExpr    :: Expr p t a
    } -> Expr p t a
  ENum :: {
      _eAnot  :: a
    , _eNum   :: XNum p t
    , _eNumOp :: NumOp t
    , _eExprL :: Expr p t a
    , _eExprR :: Expr p t a
    } -> Expr p t a
  EEqu :: {
      _eBAnot  :: a
    , _eXEqu   :: XEqu p t
    , _eBExprL :: Expr p t a
    , _eBExprR :: Expr p t a
    } -> BExpr p a
  ENEq :: {
      _eBAnot  :: a
    , _eNEqu   :: XNEqu p t
    , _eBExprL :: Expr p t a
    , _eBExprR :: Expr p t a
    } -> BExpr p a
  ELT ::
    CanCmp t => {
      _eBAnot :: a
    , _eXLT   :: XLT p t
    , _eBExprL :: Expr p t a
    , _eBExprR :: Expr p t a
    } -> BExpr p a
  EGT ::
    CanCmp t => {
      _eBAnot :: a
    , _eXGT   :: XGT p t
    , _eBExprL :: Expr p t a
    , _eBExprR :: Expr p t a
    } -> BExpr p a
  ELE ::
    CanCmp t => {
      _eBAnot :: a
    , _eXLE   :: XLE p t
    , _eBExprL :: Expr p t a
    , _eBExprR :: Expr p t a
    } -> BExpr p a
  EGE ::
    CanCmp t => {
      _eBAnot :: a
    , _eXGE   :: XGE p t
    , _eBExprL :: Expr p t a
    , _eBExprR :: Expr p t a
    } -> BExpr p a
  EAnd :: {
      _eBAnot :: a
    , _eXAnd :: XAnd p
    , _eExprBL :: BExpr p a
    , _eExprBR :: BExpr p a
    } -> BExpr p a
  EOr  :: {
      _eBAnot :: a
    , _eXOr :: XOr  p
    , _eExprBL :: BExpr p a
    , _eExprBR :: BExpr p a
    } -> BExpr p a
  ENot :: {
      _eBAnot  :: a
    , _eXNot  :: XNot p
    , _eExprB :: BExpr p a
    } -> BExpr p a
  EPostInc ::
    CanStep t => {
      _eAnot    :: a
    , _eXPostInc :: XPostInc p t
    , _eExpr    :: Expr p t a
    } -> Expr p t a
  EPostDec ::
    CanStep t => {
      _eAnot     :: a
    , _eXPostDec :: XPostDec p t
    , _eExpr     :: Expr p t a
    } -> Expr p t a
  EPreInc  ::
    CanStep t => {
      _eAnot   :: a
    , _ePreInc :: XPreInc  p t
    , _eExpr   :: Expr p t a
    } -> Expr p t a
  EPreDec  ::
    CanStep t => {
      _eAnot   :: a
    , _ePreDec :: XPreDec  p t
    , _eExpr   :: Expr p t a
    } -> Expr p t a
  EBCompl ::
    CanBitM t => {
      _eAnot    :: a
    , _eXBCompl :: XBCompl p t
    , _eExpr    :: Expr p t a
    } -> Expr p t a
  EMinus ::
    CanSub t => {
      _eAnot   :: a
    , _eXMinus :: XMinus p t
    , _eExpr   :: Expr p t a
    } -> Expr p t a
  EMApp :: {
      _eAnot  :: a
    , _eXMApp :: XMApp p t
    , _eName  :: Name
    , _eArgs  :: [TypedExpr p a]
    } -> Expr p t a
  EArrNew :: Min1 (s1 :+ s2) => a -> XArrNew p t -> XDefType p t
          -> SNat s1 -> IxEVec s1 p a
          -> SNat s2 -> AExpr (s1 :+ s2) p t a
  EArrNewI :: a -> XArrNew p t -> XTEType p t -> SNat s ->
          Proxy t -> ArrayInit s p t a -> AExpr s p t a
  ESysOut :: {
      _eVAnot   :: a
    , _eXSysOut :: XSysOut p
    , _eArg     :: TypedExpr p a
    } -> VExpr p a
  ESysErr :: {
      _eVAnot   :: a
    , _eXSysErr :: XSysErr p
    , _eArg     :: TypedExpr p a
    } -> VExpr p a

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

type family XSStmt     p
type family XSEmpty    p
type family XSBlock    p
type family XSExpr     p
type family XSVars     p
type family XSReturn   p
type family XSVReturn  p
type family XSIf       p
type family XSIfElse   p
type family XSWhile    p
type family XSDo       p
type family XSForB     p
type family XSForE     p
type family XSSwitch   p
type family XSBreak    p
type family XSContinue p
--type family XSLClass p

data Block p a = Block [Stmt p a]

data VarDeclId (s :: Nat) p where
  VarDId  :: Ident -> VarDeclId s p
  VarDArr :: Min1 s => Ident -> SNat s -> VarDeclId s p

{-
instance Eq (VarDeclId s p) where
  VarDId  i   == VarDId  j   = i == j
  VarDArr i n == VarDArr j m = i == j && n == m
-}


data VarDecl p t a where
  VarDecl :: VarDeclId s p -> Maybe (XVarInit s p t a) -> VarDecl p t a

data VarMod = VMFinal | VMNormal
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

type VVDecl n p t a = SVec n (VarDecl p t a)

data TypedVVDecl n p a where
  (::=) :: VarMod -> XTEType p t -> VVDecl n p t a -> TypedVVDecl n p a

data Stmt p a where
  SStmt     :: {
      _sAnot   :: a
    , _sXSStmt :: XSStmt p
    } -> Stmt p a
  SEmpty    :: {
      _sAnot    :: a
    , _sXSEmpty :: XSEmpty p
    } -> Stmt p a
  SBlock    :: {
      _sAnot   :: a
    , _sSBlock :: XSBlock p
    , _sBlock  :: Block p a
    } -> Stmt p a
  SExpr     :: {
      _sAnot   :: a
    , _sXSExpr :: XSExpr p
    , _sExpr   :: TypedExpr p a
    } -> Stmt p a
  SVars     ::
    Min1 n => {
      _sAnot    :: a
    , _sXSVars  :: XSVars p
    , _sTVVDecl :: TypedVVDecl n p a
    } -> Stmt p a
  SReturn   :: {
      _sAnot     :: a
    , _sXSReturn :: XSReturn p
    , _sExpr     :: TypedExpr p a
    } -> Stmt p a
  SVReturn  :: {
      _sAnot      :: a
    , _sXSVReturn :: XSVReturn p
    } -> Stmt p a
  SIf       :: {
      _sAnot :: a
    , _sXSIf :: XSIf p
    , _sCond :: BExpr p a
    , _sSi   :: Stmt p a
    } -> Stmt p a
  SIfElse   :: {
      _sAnot     :: a
    , _sXSIfElse :: XSIfElse p
    , _sCond     :: BExpr p a
    , _sSi       :: Stmt p a
    , _sSe       :: Stmt p a
    } -> Stmt p a
  SWhile    :: {
      _sAnot    :: a
    , _sXSWhile :: XSWhile p
    , _sCond    :: BExpr p a
    , _sSi      :: Stmt p a
    } -> Stmt p a
  SDo       :: {
      _sAnot :: a
    , _sXSDo :: XSDo p
    , _sCond :: BExpr p a
    , _sSi   :: Stmt p a
    } -> Stmt p a
  SForB     :: {
      _sAnot    :: a
    , _sXSForB  :: XSForB p
    , _sForInit :: Maybe (ForInit p a)
    , _sFCond   :: Maybe (BExpr p a)
    , _sFUpdate :: Maybe (VTExpr n p a)
    , _sSi      :: Stmt p a
    } -> Stmt p a
  SForE     :: {
      _sAnot   :: a
    , _sXSForE :: XSForE p
    , _sVarMod :: VarMod
    , _sIdent  :: Ident
    , _sExpr   :: TypedExpr p a
    , _sSi     :: Stmt p a
    } -> Stmt p a
  SContinue :: {
      _sAnot       :: a
    , _sXSContinue :: XSContinue p
    } -> Stmt p a
  SBreak    :: {
      _sAnot    :: a
    , _sXSBreak :: XSBreak p
    } -> Stmt p a
  SSwitch   :: {
      _sAnot :: a
    , _sXSSwitch :: XSSwitch p
    , _sTExpr    :: (XTEType p t, Expr p t a)
    , _sSwitchBs :: [SwitchBlock p t a]
    } -> Stmt p a

data SwitchBlock p t a = SwitchBlock (SwitchLabel p t a) (Block p a)

data SwitchLabel p t a
  = SwitchCase (Expr p t a)
  | Default

type VTExpr n p a = SVec n (TypedExpr p a)

-- | Initialization code for a basic @for@ statement.
data ForInit p a where
  FIVars :: Min1 n => TypedVVDecl n p a -> ForInit p a
  FIExps :: Min1 n => VTExpr n p a -> ForInit p a

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

data MemberDecl p a where
  MethodDecl :: Maybe (XDefType p t) -> Ident -> [FormalParam p a] -> Block p a
    -> MemberDecl p a

data FormalParam p a where
  FormalParam :: VarMod -> XDefType p t -> VarDeclId s p -> FormalParam p a

newtype MethodBody p a = MethodBody (Block p a)

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

data CompilationUnit p a = CompilationUnit [TypeDecl p a]

data TypeDecl p a = ClassTypeDecl (ClassDecl p a)

data ClassDecl p a = ClassDecl Ident (ClassBody p a)

newtype ClassBody p a = ClassBody [Decl p a]

data Decl p a = MemberDecl (MemberDecl p a)