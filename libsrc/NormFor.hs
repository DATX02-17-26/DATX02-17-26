module NormFor where

import CoreS.AST
import Data.Maybe

--start at compunit, go inte each classbody
execute :: CompilationUnit -> Maybe CompilationUnit
execute (CompilationUnit tds) = Just (CompilationUnit (map intoClassbody tds))

--start at classBody, go into each methodbody
intoClassbody :: TypeDecl -> TypeDecl
intoClassbody (ClassTypeDecl (ClassDecl ident (ClassBody decls))) =
  (ClassTypeDecl (ClassDecl ident (ClassBody (map intoMethodbody decls))))

--start at methodBody, go into each stmt
intoMethodbody :: Decl -> Decl
intoMethodbody (MemberDecl (MethodDecl mt ident fp (Block stmts))) =
  (MemberDecl (MethodDecl mt ident fp (Block (map intoStmt stmts))))

--change to case instead, looks like hell at the moment.
intoStmt :: Stmt -> Stmt
intoStmt (SBlock block) = undefined
intoStmt (SIf expr stmt) = (SIf expr (intoStmt stmt))
intoStmt (SIfElse expr stmt1 stmt2) =
  SIfElse expr (intoStmt stmt1) (intoStmt stmt2)
intoStmt (SWhile expr stmt) = SWhile expr $ intoStmt stmt
intoStmt (SDo expr stmt) = undefined
intoStmt (SSwitch expr [swBlock]) =undefined
intoStmt (SForB mForInit mExpr mExprs stmt) =
   SBlock $ Block [SWhile (fromMaybe (ELit (Boolean True)) mExpr) (addExpsIntoStmt mExprs stmt)]
intoStmt (SForE vmType id expr stm) =undefined
intoStmt stmt = stmt

--makes maybe exprs into statements and puts them in same scope
addExpsIntoStmt :: Maybe [Expr] -> Stmt -> Stmt
addExpsIntoStmt mExprs stmt = case mExprs of
  Nothing -> stmt
  (Just exprs) -> SBlock $ Block ([stmt] ++ (map SExpr exprs))
