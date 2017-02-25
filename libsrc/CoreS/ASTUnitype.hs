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
  | SwitchBlock CAST.SwitchLabel [AST]
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

convertCompilationUnit :: CAST.CompilationUnit -> AST
convertCompilationUnit (CAST.CompilationUnit tds) = CompilationUnit (map convertTypeDecl tds)

convertTypeDecl :: CAST.TypeDecl -> AST
convertTypeDecl (CAST.ClassTypeDecl cls) = ClassTypeDecl (convertClassDecl cls) 

convertClassDecl :: CAST.ClassDecl -> AST
convertClassDecl (CAST.ClassDecl i body) = ClassDecl i (convertClassBody body)

convertClassBody :: CAST.ClassBody -> AST
convertClassBody (CAST.ClassBody decls) = ClassBody (map convertDecl decls)

convertDecl :: CAST.Decl -> AST
convertDecl (CAST.MemberDecl m) = MemberDecl (convertMemberDecl m)

convertMemberDecl :: CAST.MemberDecl -> AST
convertMemberDecl (CAST.MethodDecl m i fmparms (CAST.Block bs)) = MethodDecl m i (map convertFormalParam fmparms) (map convertStmt bs)

convertFormalParam :: CAST.FormalParam -> AST
convertFormalParam (CAST.FormalParam a b) = FormalParam a b

convertStmt :: CAST.Stmt -> AST
convertStmt CAST.SEmpty = SEmpty
convertStmt (CAST.SBlock (CAST.Block bs)) = SBlock (map convertStmt bs)
convertStmt (CAST.SExpr expr) = SExpr (convertExpr expr)
convertStmt (CAST.SVars t) = SVars t
convertStmt (CAST.SReturn expr) = SReturn (convertExpr expr)
convertStmt CAST.SVReturn = SVReturn
convertStmt (CAST.SIf expr stmt) = SIf (convertExpr expr) (convertStmt stmt)
convertStmt (CAST.SIfElse expr stmt stmt') = SIfElse (convertExpr expr) (convertStmt stmt) (convertStmt stmt')
convertStmt (CAST.SWhile expr stmt) = SWhile (convertExpr expr) (convertStmt stmt)
convertStmt (CAST.SDo expr stmt) = SDo (convertExpr expr) (convertStmt stmt)
convertStmt (CAST.SForB mforInit mExpr mExprLst stmt) = SForB (convertForInit <$> mforInit) (convertExpr <$> mExpr) (map convertExpr <$> mExprLst) (convertStmt stmt)
convertStmt (CAST.SForE t i expr stmt) = SForE t i (convertExpr expr) (convertStmt stmt)
convertStmt CAST.SContinue = SContinue
convertStmt CAST.SBreak = SBreak
convertStmt (CAST.SSwitch expr lst) = SSwitch (convertExpr expr) (convertSwitchBlock <$> lst)

convertExpr :: CAST.Expr -> AST
convertExpr (CAST.ELit l) = ELit (convertLiteral l)
convertExpr (CAST.EVar lvalue) = EVar (convertLValue lvalue)
convertExpr (CAST.ECast t expr) = ECast t (convertExpr expr)
convertExpr (CAST.ECond e1 e2 e3) = ECond (convertExpr e1) (convertExpr e2) (convertExpr e3)
convertExpr (CAST.EAssign lvalue e) = EAssign (convertLValue lvalue) (convertExpr e)
convertExpr (CAST.EOAssign lvalue nop expr) = EOAssign (convertLValue lvalue) nop (convertExpr expr)
convertExpr (CAST.ENum op e1 e2) = ENum op (convertExpr e1) (convertExpr e2) 
convertExpr (CAST.ECmp op e1 e2) = ECmp op (convertExpr e1) (convertExpr e2) 
convertExpr (CAST.ELog op e1 e2) = ELog op (convertExpr e1) (convertExpr e2) 
convertExpr (CAST.ENot e) = ENot (convertExpr e)
convertExpr (CAST.EStep sop e) = EStep sop (convertExpr e)
convertExpr (CAST.EBCompl  e) = EBCompl $ convertExpr e 
convertExpr (CAST.EPlus    e) = EPlus  $ convertExpr e 
convertExpr (CAST.EMinus   e) = EMinus $ convertExpr e 
convertExpr (CAST.EMApp n es) = EMApp n (map convertExpr es)
convertExpr (CAST.EArrNew  t es i) = EArrNew t (map convertExpr es) i
convertExpr (CAST.EArrNewI t i ai) = EArrNewI t i (convertArrInit ai)
convertExpr (CAST.ESysOut  expr) = ESysOut (convertExpr expr)

convertLValue :: CAST.LValue -> AST
convertLValue (CAST.LVName i) = LVName i
convertLValue (CAST.LVArray e es) = LVArray (convertExpr e) (map convertExpr es)

convertLiteral :: CAST.Literal -> AST
convertLiteral (CAST.Int i) = Int i 
convertLiteral (CAST.Word i) = Word i  
convertLiteral (CAST.Float d) = Float d 
convertLiteral (CAST.Double d) = Double d 
convertLiteral (CAST.Boolean b) = Boolean b 
convertLiteral (CAST.Char c)    = Char c 
convertLiteral (CAST.String s)  = String s 
convertLiteral CAST.Null        = Null

convertArrInit :: CAST.ArrayInit -> [AST]
convertArrInit (CAST.ArrayInit xs) = map convertVarInit xs

convertVarInit :: CAST.VarInit -> AST
convertVarInit (CAST.InitExpr e) = InitExpr (convertExpr e)
convertVarInit (CAST.InitArr e) = InitArr (convertArrInit e)

convertForInit :: CAST.ForInit -> AST
convertForInit (CAST.FIVars v) = FIVars v
convertForInit (CAST.FIExprs es) = FIExprs (map convertExpr es)

convertSwitchBlock :: CAST.SwitchBlock -> AST
convertSwitchBlock (CAST.SwitchBlock l (CAST.Block bs)) = SwitchBlock l (map convertStmt bs)
