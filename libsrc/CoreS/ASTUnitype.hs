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

import qualified CoreS.AST as CAST

data AST =
    Int Integer
  | Word Integer
  | Float Double
  | Double Double
  | Boolean Bool
  | Char Char
  | String String
  | Null
  | LVName CAST.Ident
  | LVArray AST [AST]
  | InitExpr AST 
  | InitArr  [AST]
  | ELit AST
  | EVar AST 
  | ECast CAST.Type AST 
  | ECond AST AST AST
  | EAssign AST AST
  | EOAssign AST CAST.NumOp AST 
  | ENum CAST.NumOp AST AST 
  | ECmp CAST.CmpOp AST AST 
  | ELog CAST.LogOp AST AST 
  | ENot AST 
  | EStep CAST.StepOp AST 
  | EBCompl AST 
  | EPlus   AST 
  | EMinus  AST 
  | EMApp CAST.Name [AST]
  | EArrNew  CAST.Type [AST] Integer
  | EArrNewI CAST.Type Integer [AST] 
  | ESysOut  AST 
  | SEmpty
  | SBlock [AST]
  | SExpr AST 
  | SVars CAST.TypedVVDecl
  | SReturn AST 
  | SVReturn
  | SIf AST AST 
  | SIfElse AST AST AST
  | SWhile AST AST
  | SDo AST AST
  | SForB (Maybe AST) (Maybe AST) (Maybe [AST]) AST
  | SForE CAST.VMType CAST.Ident AST AST
  | SContinue
  | SBreak
  | SSwitch AST [AST]
  | SwitchBlock CAST.SwitchLabel AST 
  | SwitchCase AST
  | Default
  | FIVars  CAST.TypedVVDecl
  | FIExprs [AST]
  | MethodDecl (Maybe CAST.Type) CAST.Ident [AST] [AST] 
  | FormalParam CAST.VMType CAST.VarDeclId
  | MethodBody [AST]
  | CompilationUnit [AST]
  | ClassTypeDecl AST
  | ClassDecl CAST.Ident AST
  | ClassBody [AST]
  | MemberDecl AST
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
