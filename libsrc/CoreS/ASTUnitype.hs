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
  | MethodDecl (Maybe CAST.Type) CAST.Ident [CAST.FormalParam] AST
  | CompilationUnit AST
  | ClassTypeDecl AST
  | ClassDecl CAST.Ident AST
  | ClassBody AST
  | MemberDecl AST
  | Hole Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

convertCompilationUnit :: CAST.CompilationUnit -> AST
convertCompilationUnit (CAST.CompilationUnit tds) = CompilationUnit (Block (map convertTypeDecl tds))
convertCompilationUnit (CAST.HoleCompilationUnit i) = Hole i

convertCompilationUnitI :: AST -> CAST.CompilationUnit
convertCompilationUnitI (CompilationUnit (Block tds)) = CAST.CompilationUnit (map convertTypeDeclI tds)
convertCompilationUnitI (Hole i) = CAST.HoleCompilationUnit i 

convertTypeDecl :: CAST.TypeDecl -> AST
convertTypeDecl (CAST.ClassTypeDecl cls) = ClassTypeDecl (convertClassDecl cls) 
convertTypeDecl (CAST.HoleTypeDecl i) = Hole i

convertTypeDeclI :: AST -> CAST.TypeDecl
convertTypeDeclI (ClassTypeDecl cls) = CAST.ClassTypeDecl (convertClassDeclI cls)
convertTypeDeclI (Hole i) = CAST.HoleTypeDecl i

convertClassDecl :: CAST.ClassDecl -> AST
convertClassDecl (CAST.ClassDecl i body) = ClassDecl i (convertClassBody body)
convertClassDecl (CAST.HoleClassDecl i) = Hole i

convertClassDeclI :: AST -> CAST.ClassDecl
convertClassDeclI (ClassDecl i body) = CAST.ClassDecl i (convertClassBodyI body)
convertClassDeclI (Hole i) = CAST.HoleClassDecl i

convertClassBody :: CAST.ClassBody -> AST
convertClassBody (CAST.ClassBody decls) = ClassBody (Block (map convertDecl decls))
convertClassBody (CAST.HoleClassBody i) = Hole i

convertClassBodyI :: AST -> CAST.ClassBody
convertClassBodyI (ClassBody (Block decls)) = CAST.ClassBody (map convertDeclI decls)
convertClassBodyI (Hole i) = CAST.HoleClassBody i

convertDecl :: CAST.Decl -> AST
convertDecl (CAST.MemberDecl m) = MemberDecl (convertMemberDecl m)
convertDecl (CAST.HoleDecl i)   = Hole i

convertDeclI :: AST -> CAST.Decl  
convertDeclI (MemberDecl m) = CAST.MemberDecl (convertMemberDeclI m)
convertDeclI (Hole i)       = CAST.HoleDecl i

convertMemberDecl :: CAST.MemberDecl -> AST
convertMemberDecl (CAST.MethodDecl m i fmparms (CAST.Block bs)) =
  MethodDecl m i fmparms (Block (map convertStmt bs))
convertMemberDecl (CAST.HoleMemberDecl i) = Hole i

convertMemberDeclI :: AST -> CAST.MemberDecl
convertMemberDeclI (MethodDecl m i fmparms (Block bs)) =
  CAST.MethodDecl m i fmparms (CAST.Block (map convertStmtI bs))
convertMemberDeclI (Hole i) = CAST.HoleMemberDecl i

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
convertStmt (CAST.HoleStmt i) = Hole i

convertStmtI :: AST -> CAST.Stmt
convertStmtI SEmpty = CAST.SEmpty
convertStmtI (Block bs) = CAST.SBlock (CAST.Block (map convertStmtI bs))
convertStmtI (SExpr expr) = CAST.SExpr (convertExprI expr)
convertStmtI (SVars t) = CAST.SVars t
convertStmtI (SReturn expr) = CAST.SReturn (convertExprI expr)
convertStmtI SVReturn = CAST.SVReturn
convertStmtI (SIf expr stmt) = CAST.SIf (convertExprI expr) (convertStmtI stmt)
convertStmtI (SIfElse expr stmt stmt') = CAST.SIfElse (convertExprI expr) (convertStmtI stmt) (convertStmtI stmt')
convertStmtI (SWhile expr stmt) = CAST.SWhile (convertExprI expr) (convertStmtI stmt)
convertStmtI (SDo expr stmt) = CAST.SDo (convertExprI expr) (convertStmtI stmt)
convertStmtI (SForB mforInit mExpr mExprLst stmt) = CAST.SForB (convertForInitI <$> mforInit) (convertExprI <$> mExpr) (map convertExprI <$> mExprLst) (convertStmtI stmt)
convertStmtI (SForE t i expr stmt) = CAST.SForE t i (convertExprI expr) (convertStmtI stmt)
convertStmtI SContinue = CAST.SContinue
convertStmtI SBreak = CAST.SBreak
convertStmtI (SSwitch expr lst) = CAST.SSwitch (convertExprI expr) (convertSwitchBlockI <$> lst)
convertStmtI (Hole i) = CAST.HoleStmt i

convertExpr :: CAST.Expr -> AST
convertExpr (CAST.ELit l)                   = ELit (convertLiteral l)
convertExpr (CAST.EVar lvalue)              = EVar (convertLValue lvalue)
convertExpr (CAST.ECast t expr)             = ECast t (convertExpr expr)
convertExpr (CAST.ECond e1 e2 e3)           = ECond (convertExpr e1) (convertExpr e2) (convertExpr e3)
convertExpr (CAST.EAssign lvalue e)         = EAssign (convertLValue lvalue) (convertExpr e)
convertExpr (CAST.EOAssign lvalue nop expr) = EOAssign (convertLValue lvalue) nop (convertExpr expr)
convertExpr (CAST.ENum op e1 e2)            = ENum op (convertExpr e1) (convertExpr e2) 
convertExpr (CAST.ECmp op e1 e2)            = ECmp op (convertExpr e1) (convertExpr e2) 
convertExpr (CAST.ELog op e1 e2)            = ELog op (convertExpr e1) (convertExpr e2) 
convertExpr (CAST.ENot e)                   = ENot (convertExpr e)
convertExpr (CAST.EStep sop e)              = EStep sop (convertExpr e)
convertExpr (CAST.EBCompl  e)               = EBCompl $ convertExpr e 
convertExpr (CAST.EPlus    e)               = EPlus  $ convertExpr e 
convertExpr (CAST.EMinus   e)               = EMinus $ convertExpr e 
convertExpr (CAST.EMApp n es)               = EMApp n (map convertExpr es)
convertExpr (CAST.EArrNew  t es i)          = EArrNew t (map convertExpr es) i
convertExpr (CAST.EArrNewI t i ai)          = EArrNewI t i (convertArrInit ai)
convertExpr (CAST.ESysOut  expr)            = ESysOut (convertExpr expr)
convertExpr (CAST.HoleExpr i)               = Hole i

convertExprI :: AST -> CAST.Expr
convertExprI (ELit l)                   = CAST.ELit (convertLiteralI l)
convertExprI (EVar lvalue)              = CAST.EVar (convertLValueI lvalue)
convertExprI (ECast t expr)             = CAST.ECast t (convertExprI expr)
convertExprI (ECond e1 e2 e3)           = CAST.ECond (convertExprI e1) (convertExprI e2) (convertExprI e3)
convertExprI (EAssign lvalue e)         = CAST.EAssign (convertLValueI lvalue) (convertExprI e)
convertExprI (EOAssign lvalue nop expr) = CAST.EOAssign (convertLValueI lvalue) nop (convertExprI expr)
convertExprI (ENum op e1 e2)            = CAST.ENum op (convertExprI e1) (convertExprI e2) 
convertExprI (ECmp op e1 e2)            = CAST.ECmp op (convertExprI e1) (convertExprI e2) 
convertExprI (ELog op e1 e2)            = CAST.ELog op (convertExprI e1) (convertExprI e2) 
convertExprI (ENot e)                   = CAST.ENot (convertExprI e)
convertExprI (EStep sop e)              = CAST.EStep sop (convertExprI e)
convertExprI (EBCompl  e)               = CAST.EBCompl $ convertExprI e 
convertExprI (EPlus    e)               = CAST.EPlus  $ convertExprI e 
convertExprI (EMinus   e)               = CAST.EMinus $ convertExprI e 
convertExprI (EMApp n es)               = CAST.EMApp n (map convertExprI es)
convertExprI (EArrNew  t es i)          = CAST.EArrNew t (map convertExprI es) i
convertExprI (EArrNewI t i ai)          = CAST.EArrNewI t i (convertArrInitI ai)
convertExprI (ESysOut  expr)            = CAST.ESysOut (convertExprI expr)
convertExprI (Hole i)                   = CAST.HoleExpr i

convertLValue :: CAST.LValue -> AST
convertLValue (CAST.LVName i)     = LVName i
convertLValue (CAST.LVArray e es) = LVArray (convertExpr e) (map convertExpr es)
convertLValue (CAST.HoleLValue i) = Hole i

convertLValueI :: AST -> CAST.LValue
convertLValueI (LVName i)     = CAST.LVName i
convertLValueI (LVArray e es) = CAST.LVArray (convertExprI e) (map convertExprI es)
convertLValueI (Hole i)       = CAST.HoleLValue i

convertLiteral :: CAST.Literal -> AST
convertLiteral (CAST.Int i)         = Int i 
convertLiteral (CAST.Word i)        = Word i  
convertLiteral (CAST.Float d)       = Float d 
convertLiteral (CAST.Double d)      = Double d 
convertLiteral (CAST.Boolean b)     = Boolean b 
convertLiteral (CAST.Char c)        = Char c 
convertLiteral (CAST.String s)      = String s 
convertLiteral CAST.Null            = Null
convertLiteral (CAST.HoleLiteral i) = Hole i

convertLiteralI :: AST -> CAST.Literal
convertLiteralI (Int i)     = CAST.Int i 
convertLiteralI (Word i)    = CAST.Word i  
convertLiteralI (Float d)   = CAST.Float d 
convertLiteralI (Double d)  = CAST.Double d 
convertLiteralI (Boolean b) = CAST.Boolean b 
convertLiteralI (Char c)    = CAST.Char c 
convertLiteralI (String s)  = CAST.String s 
convertLiteralI Null        = CAST.Null
convertLiteralI (Hole i)    = CAST.HoleLiteral i

convertArrInit :: CAST.ArrayInit -> [AST]
convertArrInit (CAST.ArrayInit xs) = map convertVarInit xs

convertArrInitI :: [AST] -> CAST.ArrayInit
convertArrInitI xs = CAST.ArrayInit (map convertVarInitI xs)

convertVarInit :: CAST.VarInit -> AST
convertVarInit (CAST.InitExpr e)    = InitExpr (convertExpr e)
convertVarInit (CAST.InitArr e)     = InitArr (convertArrInit e)
convertVarInit (CAST.HoleVarInit i) = Hole i

convertVarInitI :: AST -> CAST.VarInit
convertVarInitI (InitExpr e) = CAST.InitExpr (convertExprI e)
convertVarInitI (InitArr e)  = CAST.InitArr  (convertArrInitI e)
convertVarInitI (Hole i)     = CAST.HoleVarInit i

convertForInit :: CAST.ForInit -> AST
convertForInit (CAST.FIVars v)      = FIVars v
convertForInit (CAST.FIExprs es)    = FIExprs (map convertExpr es)
convertForInit (CAST.HoleForInit i) = Hole i

convertForInitI :: AST -> CAST.ForInit
convertForInitI (FIVars v)   = CAST.FIVars v
convertForInitI (FIExprs es) = CAST.FIExprs (map convertExprI es)
convertForInitI (Hole i)     = CAST.HoleForInit i

convertSwitchBlock :: CAST.SwitchBlock -> AST
convertSwitchBlock (CAST.SwitchBlock l (CAST.Block bs)) = SwitchBlock l (map convertStmt bs)
convertSwitchBlock (CAST.HoleSwitchBlock i)             = Hole i

convertSwitchBlockI :: AST -> CAST.SwitchBlock
convertSwitchBlockI (SwitchBlock l stmts) = CAST.SwitchBlock l (CAST.Block (map convertStmtI stmts))
convertSwitchBlockI (Hole i)              = CAST.HoleSwitchBlock i

matchList :: [AST] -> [AST] -> Bool
matchList as bs
  | length as == length bs = and [canMatch a b | (a, b) <- zip as bs]
  | otherwise = False

-- | `canMatch complete incomplete` Tells us if the complete AST can possibly match the incomplete AST
--
-- todo: refactor....
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
canMatch (LVArray a as) (LVArray b bs) = canMatch a b && matchList as bs
canMatch (InitExpr ast) (InitExpr ast') = canMatch ast ast' 
canMatch (InitArr  as) (InitArr bs) = matchList as bs
canMatch (ELit ast) (ELit ast') = canMatch ast ast' 
canMatch (EVar ast) (EVar ast') = canMatch ast ast' 
canMatch (ECast t ast) (ECast t' ast') = t == t' && canMatch ast ast' 
canMatch (ECond a b c) (ECond d e f) = canMatch a d && canMatch b e && canMatch c f
canMatch (EAssign a b) (EAssign c d) = canMatch a c && canMatch b d
canMatch (EOAssign ast op ast') (EOAssign ast'' op' ast''') = op == op' && canMatch ast ast'' && canMatch ast' ast'''
canMatch (ENum op ast ast') (ENum op' ast'' ast''') = op == op' && canMatch ast ast'' && canMatch ast' ast'''
canMatch (ECmp op ast bst) (ECmp op' ast' bst') = op == op' && canMatch ast ast' && canMatch bst bst' 
canMatch (ELog op ast bst) (ELog op' ast' bst') = op == op' && canMatch ast ast' && canMatch bst bst' 
canMatch (ENot ast) (ENot ast') = canMatch ast ast'
canMatch (EStep op ast) (EStep op' ast') = op == op' && canMatch ast ast' 
canMatch (EBCompl ast) (EBCompl ast') = canMatch ast ast' 
canMatch (EPlus ast) (EPlus ast') = canMatch ast ast' 
canMatch (EMinus ast) (EMinus ast') = canMatch ast ast' 
canMatch (EMApp c asts) (EMApp d asts') = c == d && matchList asts asts'
canMatch (EArrNew  t xs is) (EArrNew  t' xs' is') = t == t' && matchList xs xs' && is == is'
canMatch (EArrNewI t i xs) (EArrNewI t' i' xs') = t == t' && i == i' && matchList xs xs'
canMatch (ESysOut ast) (ESysOut ast') = canMatch ast ast'
canMatch SEmpty SEmpty = True
canMatch (Block ast) (Block ast') = matchList ast ast'
canMatch (SExpr ast) (SExpr ast') = canMatch ast ast' 
canMatch (SVars t) (SVars t') = t == t'
canMatch (SReturn ast) (SReturn ast') = canMatch ast ast' 
canMatch SVReturn SVReturn = True
canMatch (SIf a b) (SIf c d) = canMatch a c && canMatch c d
canMatch (SIfElse b f s) (SIfElse b' f' s') = canMatch b b' && canMatch f f' && canMatch s s'
canMatch (SWhile b bd) (SWhile b' bd') = canMatch b b' && canMatch bd bd'
canMatch (SDo b bd) (SDo b' bd') = canMatch b b' && canMatch bd bd'
canMatch (SForB ma mb mcs d) (SForB ma' mb' mcs' d') = canMatch d d' && (ma == ma' || maybe False id (canMatch <$> ma <*> ma')) &&
                                                       (mb == mb' || maybe False id (canMatch <$> mb <*> mb')) &&
                                                       (mcs == mcs' || maybe False id (matchList <$> mcs <*> mcs'))
canMatch (SForE t i a b) (SForE t' i' a' b') = t == t' && i == i' && canMatch a a' && canMatch b b' 
canMatch SContinue SContinue = True
canMatch SBreak SBreak = True
canMatch (SSwitch ast asts) (SSwitch ast' asts') = canMatch ast ast' && matchList asts asts'
canMatch (SwitchBlock l asts) (SwitchBlock l' asts') = l == l' && matchList asts asts'
canMatch (SwitchCase ast) (SwitchCase ast') = canMatch ast ast' 
canMatch Default Default = True
canMatch (FIVars i) (FIVars j) = i == j
canMatch (FIExprs as) (FIExprs bs) = matchList as bs
canMatch (MethodDecl t id xs ast) (MethodDecl t' id' xs' ast') = t == t' && id == id' && xs == xs' && canMatch ast ast'
canMatch (CompilationUnit a) (CompilationUnit b) = canMatch a b
canMatch (ClassTypeDecl ast) (ClassTypeDecl ast') = canMatch ast ast' 
canMatch (ClassDecl i ast) (ClassDecl j ast') = i == j && canMatch ast ast' 
canMatch (ClassBody ast) (ClassBody ast') = canMatch ast ast' 
canMatch (MemberDecl ast) (MemberDecl ast') = canMatch ast ast'
canMatch _ _ = False
