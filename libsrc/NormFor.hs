module NormFor where

import CoreS.AST
import Data.Maybe

--start at compunit, go inte each classbody
execute :: CompilationUnit -> Maybe CompilationUnit
execute (CompilationUnit tds) = Just $ CompilationUnit $ intoClassbody <$> tds

--start at classBody, go into each methodbody
intoClassbody :: TypeDecl -> TypeDecl
intoClassbody (ClassTypeDecl (ClassDecl ident (ClassBody decls))) =
  ClassTypeDecl $ ClassDecl ident $ ClassBody $ intoMethodbody <$> decls

--start at methodBody, go into each stmt
intoMethodbody :: Decl -> Decl
intoMethodbody (MemberDecl (MethodDecl mt ident fp (Block stmts))) =
  MemberDecl $ MethodDecl mt ident fp $ Block $ intoStmt <$> stmts

{-
  looks like hell, fix with record wildcards.
  At the moment only changes for to while, adds a scope
  around it.
-}
intoStmt :: Stmt -> Stmt
intoStmt stm = case stm of
  SBlock (Block stmts)             -> SBlock $ Block $ intoStmt <$> stmts
  SIf expr stmt                    -> (SIf expr (intoStmt stmt))
  SIfElse expr stmt1 stmt2         -> SIfElse expr (intoStmt stmt1) (intoStmt stmt2)
  SWhile expr stmt                 -> SWhile expr $ intoStmt stmt
  SDo expr stmt                    -> SDo expr (intoStmt stmt)
  SForB mForInit mExpr mExprs stmt ->
    SBlock $ Block [(maybe SEmpty forInitToStmt mForInit),
                    SWhile (fromMaybe (ELit (Boolean True)) mExpr)
                    (addExpsIntoStmt mExprs (intoStmt stmt))]
  SForE vmType ident expr stmt     -> SForE vmType ident expr (intoStmt stmt)
  stmt                             -> stmt

{-
  makes maybe exprs into statements and puts them
  in same scope as the block as the stmts in a loop.
-}
addExpsIntoStmt :: Maybe [Expr] -> Stmt -> Stmt
addExpsIntoStmt mExprs stmt = case mExprs of
  Nothing -> stmt
  (Just exprs) -> SBlock $ Block ([stmt] ++ (SExpr <$> exprs))

--Depending on what kind of init, turn into stmts
forInitToStmt :: ForInit -> Stmt
forInitToStmt fInit = case fInit of
  FIVars typedVVDecl -> SVars typedVVDecl
  FIExprs exprs      -> SBlock $ Block $ SExpr <$> exprs
