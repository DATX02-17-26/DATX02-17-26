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

module CoreS.ASTUnitype where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import CoreS.AST

data AST =
    Int Integer
  | Word Integer
  | Float Double
  | Double Double
  | Boolean Bool
  | Char Char
  | String String
  | Null
  | LVName Ident
  | LVArray AST [AST]
  | InitExpr AST 
  | InitArr  [AST]
  | ELit AST
  | EVar AST 
  | ECast Type AST 
  | ECond AST AST AST
  | EAssign AST AST
  | EOAssign AST NumOp AST 
  | ENum NumOp AST AST 
  | ECmp CmpOp AST AST 
  | ELog LogOp AST AST 
  | ENot AST 
  | EStep StepOp AST 
  | EBCompl AST 
  | EPlus   AST 
  | EMinus  AST 
  | EMApp Name [AST]
  | EArrNew  Type [AST] Integer
  | EArrNewI Type Integer [AST] 
  | ESysOut  AST 
  | SEmpty
  | SBlock [AST]
  | SExpr AST 
  | SVars TypedVVDecl
  | SReturn AST 
  | SVReturn
  | SIf AST AST 
  | SIfElse AST AST AST
  | SWhile AST AST
  | SDo AST AST
  | SForB (Maybe AST) (Maybe AST) (Maybe [AST]) AST
  | SForE VMType Ident AST AST
  | SContinue
  | SBreak
  | SSwitch AST [AST]
  | SwitchBlock SwitchLabel AST 
  | SwitchCase AST
  | Default
  | FIVars  TypedVVDecl
  | FIExprs [AST]
  | MethodDecl (Maybe Type) Ident [AST] [AST] 
  | FormalParam VMType VarDeclId
  | MethodBody [AST]
  | CompilationUnit [AST]
  | ClassTypeDecl AST
  | ClassDecl Ident AST
  | ClassBody [AST]
  | MemberDecl AST
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
