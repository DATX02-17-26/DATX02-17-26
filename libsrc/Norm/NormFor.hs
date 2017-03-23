module Norm.NormFor (
    normForToWhile
  ) where

import CoreS.AST
import Data.Maybe
import Norm.NormCS hiding(stages, name, execute)

--make a rule that normalizes for into while
normForToWhile :: NormCUR
normForToWhile =  makeRule (convMayN execute) name stages

--The name of the normalization step
name :: String
name = "NormForToWhile"

--The normalization stage
stages :: [Int]
stages = [1]

--start at compunit, go inte each classbody
execute :: CompilationUnit -> Maybe CompilationUnit
execute cu = case cu of
  CompilationUnit tds ->
    let normCU = CompilationUnit $ intoClassTypeDecl <$> tds
    in if cu == normCU then Nothing else Just normCU
  hole -> Nothing

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
--to run the method where blocks are removed, add a 0 to intoBlock
intoMemberDecl :: MemberDecl -> MemberDecl
intoMemberDecl md = case md of
  MethodDecl mt ident fp block -> MethodDecl mt ident fp (intoBlock block)
  hole                         -> hole

--Block go into stmts
intoBlock :: Block -> Block
intoBlock block = case block of
  --Block stmts -> Block (goStmts stmts)
  (Block stmts) -> Block (intoStmt <$> stmts)
  hole          -> hole

{-
--This method will only work when blocks are already removed, otherwise
--it may remove blocks that are not supposed to be removed

intoBlock :: Block -> Int -> Block
intoBlock b i = case b of
  (Block bStmts) -> case (splitAt i bStmts) of
  --  (stmts1, (x:[]))   -> Block $ stmts1 ++ [intoStmt x]
    (stmts1, [])       -> Block stmts1
    (stms1, (s:stms2)) -> case intoStmt s of
      SBlock (Block [si, (SWhile e ws)]) -> intoBlock (Block (stms1 ++ [si] ++ [(SWhile e ws)] ++ stms2)) (i+1)
      stmt                               -> intoBlock (Block (stms1 ++ [stmt] ++ stms2)) (i+1)
  hole           -> hole
-}

{-
  At the moment only changes for to while, adds a scope
  around it. Should work with holes
  To run block removal: SBlock $ intoBlock block 0
-}
intoStmt :: Stmt -> Stmt
intoStmt stm = case stm of
  SBlock block                 -> SBlock $ intoBlock block
  SIf expr stmt                -> SIf expr (intoStmt stmt)
  SIfElse expr stmt1 stmt2     -> SIfElse expr (intoStmt stmt1) (intoStmt stmt2)
  SWhile expr stmt             -> SWhile expr $ intoStmt stmt
  SDo expr stmt                -> SDo expr (intoStmt stmt)
  SForB mfi me mes s           -> addBlock $ forToWhile (SForB mfi me mes s)
  SForE vmType ident expr stmt -> SForE vmType ident expr (intoStmt stmt)
  stmt                         -> stmt


--creates a block around a stmt
addBlock :: [Stmt] -> Stmt
addBlock stmt = SBlock $ Block stmt

--takes a for-loop, converts it into a while by adding a block around it
forToWhile :: Stmt -> [Stmt]
forToWhile (SForB mForInit mExpr mExprs stmt) =
  [(maybe SEmpty forInitToStmt mForInit),
  SWhile (convForCond mExpr) (addExpsIntoStmt mExprs (intoStmt stmt))]


--converts a maybe condition into a condition for a while-loop
convForCond :: Maybe Expr -> Expr
convForCond mExpr = fromMaybe (ELit (Boolean True)) mExpr

{-
  makes maybe exprs into SExprs and puts them
  in same sblock if there is one, otherwise if only 1 statement
  and no block, create a block around and add SExprs.
  Should work with holes.
-}
addExpsIntoStmt :: Maybe [Expr] -> Stmt -> Stmt
addExpsIntoStmt mExprs stmt = case mExprs of
  Nothing -> stmt
  (Just exprs) -> case stmt of
    SBlock (Block stmts) -> SBlock $ Block $ stmts ++ (SExpr <$> exprs)
    _                    -> SBlock $ Block $ [stmt] ++ (SExpr <$> exprs)

{-
  Depending on what kind of init, turn into stmts
  Should work with holes. Maybe handle hole in different way?
-}
forInitToStmt :: ForInit -> Stmt
forInitToStmt fInit = case fInit of
  FIVars typedVVDecl -> SVars typedVVDecl
  FIExprs exprs      -> SBlock $ Block $ SExpr <$> exprs
  HoleForInit i      -> HoleStmt i
