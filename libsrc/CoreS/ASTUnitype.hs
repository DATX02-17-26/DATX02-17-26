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
  | Block [AST]
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
  | FIVars CAST.TypedVVDecl
  | FIExprs [AST]
  | MethodDecl (Maybe CAST.Type) CAST.Ident [AST] AST
  | FormalParam CAST.VMType CAST.VarDeclId
  | CompilationUnit AST
  | ClassTypeDecl AST
  | ClassDecl CAST.Ident AST
  | ClassBody AST
  | MemberDecl AST
  | Hole Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

convertCompilationUnit :: CAST.CompilationUnit -> AST
convertCompilationUnit (CAST.CompilationUnit tds) = CompilationUnit (Block (map convertTypeDecl tds))

convertTypeDecl :: CAST.TypeDecl -> AST
convertTypeDecl (CAST.ClassTypeDecl cls) = ClassTypeDecl (convertClassDecl cls) 

convertClassDecl :: CAST.ClassDecl -> AST
convertClassDecl (CAST.ClassDecl i body) = ClassDecl i (convertClassBody body)

convertClassBody :: CAST.ClassBody -> AST
convertClassBody (CAST.ClassBody decls) = ClassBody (Block (map convertDecl decls))

convertDecl :: CAST.Decl -> AST
convertDecl (CAST.MemberDecl m) = MemberDecl (convertMemberDecl m)

convertMemberDecl :: CAST.MemberDecl -> AST
convertMemberDecl (CAST.MethodDecl m i fmparms (CAST.Block bs)) =
  MethodDecl m i (map convertFormalParam fmparms) (Block (map convertStmt bs))

convertFormalParam :: CAST.FormalParam -> AST
convertFormalParam (CAST.FormalParam a b) = FormalParam a b

convertStmt :: CAST.Stmt -> AST
convertStmt CAST.SEmpty = SEmpty
convertStmt (CAST.SBlock (CAST.Block bs)) = Block (map convertStmt bs)
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

-- | `canMatch complete incomplete` Tells us if the complete AST can possibly match the incomplete AST
canMatch :: AST -> AST -> Bool
canMatch _ (Hole _) = True
canMatch (Int i)  (Int j) = i == j 
canMatch (Word i) (Word j) = i == j
canMatch (Float i) (Float j) = i == j 
canMatch (Double d) (Double b) = d == b 
canMatch (Boolean b) (Boolean a) = a == b 
canMatch (Char c) (Char a) = a == c 
canMatch (String s) (String s') = s == s' 
canMatch Null Null = True
canMatch (LVName i) (LVName j) = i == j
canMatch (LVArray a as) (LVArray b bs) = canMatch b bs && and [canMatch a b | (a, b) <- zip as bs]
canMatch (InitExpr ast) (InitExpr ast') = canMatch ast ast' 
canMatch (InitArr  as) (InitArr bs) = and [canMatch a b | (a, b) <- zip as bs]
canMatch (ELit ast) (ELit ast') = canMatch ast ast' 
canMatch (EVar ast) (EVar ast') = canMatch ast ast' 
canMatch (ECast t ast) (ECast t' ast') = t == t' && canMatch ast ast' 
canMatch (ECond a b c) (ECast d e f) = canMatch a d && canMatch b e && canMatch c f
canMatch (EAssign a b) (EAssign c d) = canMatch a c && canMatch b d
canMatch (EOAssign ast op ast') (EOAssign ast'' op' ast''') = op == op' && canMatch ast ast'' && canMatch ast' ast'''
canMatch (ENum CAST.NumOp AST AST 
canMatch (ECmp CAST.CmpOp AST AST 
canMatch (ELog CAST.LogOp AST AST 
canMatch (ENot AST 
canMatch (EStep CAST.StepOp AST 
canMatch (EBCompl AST 
canMatch (EPlus   AST 
canMatch (EMinus  AST 
canMatch (EMApp CAST.Name [AST]
canMatch (EArrNew  CAST.Type [AST] Integer
canMatch (EArrNewI CAST.Type Integer [AST] 
canMatch (ESysOut  AST 
canMatch (SEmpty
canMatch (Block [AST]
canMatch (SExpr AST 
canMatch (SVars CAST.TypedVVDecl
canMatch (SReturn AST 
canMatch (SVReturn
canMatch (SIf AST AST 
canMatch (SIfElse AST AST AST
canMatch (SWhile AST AST
canMatch (SDo AST AST
canMatch (SForB (Maybe AST) (Maybe AST) (Maybe [AST]) AST
canMatch (SForE CAST.VMType CAST.Ident AST AST
canMatch (SContinue
canMatch (SBreak
canMatch (SSwitch AST [AST]
canMatch (SwitchBlock CAST.SwitchLabel [AST]
canMatch (SwitchCase AST
canMatch (Default
canMatch (FIVars CAST.TypedVVDecl
canMatch (FIExprs [AST]
canMatch (MethodDecl (Maybe CAST.Type) CAST.Ident [AST] AST
canMatch (FormalParam t d) (FormalParam b c) = t == b && d == c
canMatch (CompilationUnit a) (CompilationUnit b) = canMatch a b
canMatch (ClassTypeDecl ast) (ClassTypeDecl ast') = canMatch ast ast' 
canMatch (ClassDecl i ast) (ClassDecl j ast') = i == j && canMatch ast ast' 
canMatch (ClassBody ast) (ClassBody ast') = canMatch ast ast' 
canMatch (MemberDecl ast) (MemberDecl ast') = canMatch ast ast'
canMatch _ _ = False
