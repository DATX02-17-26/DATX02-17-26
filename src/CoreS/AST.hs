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
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data LValue
  = LVName Ident
  | LVArray Expr [Expr]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarInit
  = InitExpr Expr
  | InitArr  ArrayInit
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ArrayInit = ArrayInit [VarInit]
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
 deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarDeclId
  = VarDId  Ident
  | VarDArr Ident Integer
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarDecl = VarDecl VarDeclId (Maybe VarInit)
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarMod = VMFinal | VMNormal
  deriving (Eq, Ord, Enum,Bounded, Show, Read, Typeable, Data, Generic)

data VMType = VMType VarMod Type
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data TypedVVDecl = TypedVVDecl VMType [VarDecl]
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
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data SwitchBlock = SwitchBlock SwitchLabel Block
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data SwitchLabel
  = SwitchCase Expr
  | Default
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ForInit
  = FIVars TypedVVDecl
  | FIExps [Expr]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

data MemberDecl = MethodDecl (Maybe Type) Ident [FormalParam] Block
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data FormalParam = FormalParam VMType VarDeclId
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

newtype MethodBody = MethodBody Block
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

data CompilationUnit = CompilationUnit [TypeDecl]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data TypeDecl  = ClassTypeDecl ClassDecl
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data ClassDecl = ClassDecl Ident ClassBody
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

newtype ClassBody = ClassBody [Decl]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data Decl = MemberDecl MemberDecl
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)