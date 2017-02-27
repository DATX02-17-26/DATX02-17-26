module NormFor where

import CoreS.AST

--start att compunit, go inte each classbody
execute :: CompilationUnit -> Maybe CompilationUnit
execute (CompilationUnit tds) = Just (CompilationUnit (map intoClassbody tds))

--go into each methodbody
intoClassbody :: TypeDecl -> TypeDecl
intoClassbody (ClassTypeDecl (ClassDecl ident (ClassBody decls))) =
  (ClassTypeDecl (ClassDecl ident (ClassBody (map intoMethodbody decls))))

--go into each stmt
intoMethodbody :: Decl -> Decl
intoMethodbody (MemberDecl (MethodDecl mt ident fp (Block stmts))) =
  (MemberDecl (MethodDecl mt ident fp (Block (map intoStmt stmts))))

--change to case instead
intoStmt :: Stmt -> Stmt
intoStmt (SBlock block) = undefined
intoStmt (SIf expr stmt) =undefined
intoStmt (SIfElse expr stmt1 stmt2) =undefined
intoStmt (SWhile expr stmt) =undefined
intoStmt (SDo expr stmt) =undefined
intoStmt (SSwitch expr [swBlock]) =undefined
intoStmt (SForB mForInit mExpr mExprs stm) =undefined
intoStmt (SForE vmType id expr stm) =undefined
intoStmt stmt = stmt
