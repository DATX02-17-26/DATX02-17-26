module NormFor where

import CoreS.AST
import Data.Maybe

--start at compunit, go inte each classbody
execute :: CompilationUnit -> Maybe CompilationUnit
execute cu = case cu of
  CompilationUnit tds -> Just $ CompilationUnit $ intoClassTypeDecl <$> tds
  hole -> Just hole

--start at classBody, go into ClassDecl
intoClassTypeDecl :: TypeDecl -> TypeDecl
intoClassTypeDecl ctd = case ctd of
  ClassTypeDecl cd -> ClassTypeDecl (intoClassDecl cd)
  hole             -> hole
  --ClassTypeDecl $ ClassDecl ident $ ClassBody $ intoMethodbody <$> decls

--ClassDecl go into ClassBody
intoClassDecl :: ClassDecl -> ClassDecl
intoClassDecl cd = case cd of
  (ClassDecl ident cb) -> ClassDecl ident (intoClassBody cb)
  hole                 -> hole

--ClassBody go into Decl
intoClassBody :: ClassBody -> ClassBody
intoClassBody cb = case cb of
  ClassBody decls -> ClassBody (intoDecl <$> decls)
  hole            -> hole

--Decl go into MemberDecl
intoDecl :: Decl -> Decl
intoDecl dec = case dec of
  MemberDecl md -> MemberDecl (intoMemberDecl md)
  hole          -> hole

--MemberDecl go into Block
intoMemberDecl :: MemberDecl -> MemberDecl
intoMemberDecl md = case md of
  MethodDecl mt ident fp block -> MethodDecl mt ident fp (intoBlock block)
  hole                         -> hole

--Block go into stmts
intoBlock :: Block -> Block
intoBlock block = case block of
  (Block stmts) -> Block (intoStmt <$> stmts)
  hole          -> hole
   --(MemberDecl (MethodDecl mt ident fp (Block stmts))) =
  --MemberDecl $ MethodDecl mt ident fp $ Block $ intoStmt <$> stmts

{-
  looks like hell, fix with record wildcards.
  At the moment only changes for to while, adds a scope
  around it. Should work with holes
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
  Should work with holes.
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
  HoleForInit i      -> HoleStmt i
