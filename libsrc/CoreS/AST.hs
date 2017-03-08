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

module CoreS.AST where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Names and identifiers:
--------------------------------------------------------------------------------

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data Name = Name [Ident]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

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

--------------------------------------------------------------------------------
-- Operators:
--------------------------------------------------------------------------------


data NumOp
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  | LShift
  | RShift
  | RRShift
  | And
  | Xor
  | Or
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

data CmpOp
  = EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

data LogOp
  = LAnd
  | LOr
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

data StepOp
  = PostInc
  | PostDec
  | PreInc
  | PreDec
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Expressions:
--------------------------------------------------------------------------------

data Literal
  = Int Integer
  | Word Integer
  | Float Double
  | Double Double
  | Boolean Bool
  | Char Char
  | String String
  | Null
  | HoleLiteral Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data LValue
  = LVName Ident
  | LVArray Expr [Expr]
  | HoleLValue Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarInit
  = InitExpr Expr
  | InitArr  ArrayInit
  | HoleVarInit Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ArrayInit = ArrayInit [VarInit] | HoleArrayInit Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data Expr
  = ELit Literal
  | EVar LValue
  | ECast Type Expr
  | ECond Expr Expr Expr
  | EAssign LValue Expr
  | EOAssign LValue NumOp Expr
  | ENum NumOp Expr Expr
  | ECmp CmpOp Expr Expr
  | ELog LogOp Expr Expr
  | ENot Expr
  | EStep StepOp Expr
  | EBCompl  Expr
  | EPlus    Expr
  | EMinus   Expr
  | EMApp Name [Expr]
  | EArrNew  Type [Expr] Integer
  | EArrNewI Type Integer ArrayInit
  | ESysOut  Expr
  | HoleExpr Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

data Block = Block [Stmt] | HoleBlock Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarDeclId
  = VarDId  Ident
  | VarDArr Ident Integer
  | HoleVarDeclId Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarDecl = VarDecl VarDeclId (Maybe VarInit) | HoleVarDecl Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarMod = VMFinal | VMNormal
  deriving (Eq, Ord, Enum,Bounded, Show, Read, Typeable, Data, Generic)

data VMType = VMType VarMod Type | HoleVMType Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data TypedVVDecl = TypedVVDecl VMType [VarDecl] | HoleTypedVVDecl Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data Stmt
  = SEmpty
  | SBlock Block
  | SExpr Expr
  | SVars TypedVVDecl
  | SReturn Expr
  | SVReturn
  | SIf Expr Stmt
  | SIfElse Expr Stmt Stmt
  | SWhile Expr Stmt
  | SDo Expr Stmt
  | SForB (Maybe ForInit) (Maybe Expr) (Maybe [Expr]) Stmt
  | SForE VMType Ident Expr Stmt
  | SContinue
  | SBreak
  | SSwitch Expr [SwitchBlock]
  | HoleStmt Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data SwitchBlock = SwitchBlock SwitchLabel Block | HoleSwitchBlock Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data SwitchLabel
  = SwitchCase Expr
  | Default
  | HoleSwitchLabel Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ForInit
  = FIVars  TypedVVDecl
  | FIExprs [Expr]
  | HoleForInit Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

data MemberDecl = MethodDecl (Maybe Type) Ident [FormalParam] Block | HoleMemberDecl Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data FormalParam = FormalParam VMType VarDeclId | HoleFormalParam Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data MethodBody = MethodBody Block | HoleMethodBody Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

data CompilationUnit = CompilationUnit [TypeDecl] | HoleCompilationUnit Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data TypeDecl  = ClassTypeDecl ClassDecl | HoleTypeDecl Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ClassDecl = ClassDecl Ident ClassBody | HoleClassDecl Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ClassBody = ClassBody [Decl] | HoleClassBody Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data Decl = MemberDecl MemberDecl | HoleDecl Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
