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

{-# LANGUAGE LambdaCase #-}

-- | Normalization of for loops into while.
module Norm.NormFor (
  -- * Normalizers
    normForToWhile
  -- * Utility functions
  , mfiToStmt
  ) where

import CoreS.AST
import Data.Maybe
import Norm.NormCS hiding (stages, name, execute)

-- | make a rule that normalizes for into while
normForToWhile :: NormCUR
normForToWhile =  makeRule (convEqN execute) name stages

-- | The name of the normalization step
name :: String
name = "for.stmt.to_while"

-- | The normalization stage
stages :: [Int]
stages = [1]

-- | start at compunit, go inte each classbody
execute :: CompilationUnit -> CompilationUnit
execute = \case
  CompilationUnit is tds -> CompilationUnit is $ intoClassTypeDecl <$> tds
  hole -> hole

-- | start at classBody, go into ClassDecl
intoClassTypeDecl :: TypeDecl -> TypeDecl
intoClassTypeDecl = \case
  ClassTypeDecl cd -> ClassTypeDecl (intoClassDecl cd)
  hole             -> hole

-- | ClassDecl go into ClassBody
intoClassDecl :: ClassDecl -> ClassDecl
intoClassDecl = \case
  (ClassDecl ident cb) -> ClassDecl ident (intoClassBody cb)
  hole                 -> hole

-- | ClassBody go into Decl
intoClassBody :: ClassBody -> ClassBody
intoClassBody = \case
  ClassBody decls -> ClassBody (intoDecl <$> decls)
  hole            -> hole

-- | Decl go into MemberDecl
intoDecl :: Decl -> Decl
intoDecl = \case
  MemberDecl md -> MemberDecl (intoMemberDecl md)
  hole          -> hole

-- | MemberDecl go into Block
intoMemberDecl :: MemberDecl -> MemberDecl
intoMemberDecl = \case
  MethodDecl mt ident fp block -> MethodDecl mt ident fp (intoBlock block)
  hole                         -> hole

-- | Block go into stmts
intoBlock :: Block -> Block
intoBlock = \case
  Block stmts -> Block (intoStmt <$> stmts)
  hole        -> hole

-- | Changes for to while, adds a scope
-- around it. Should work with holes
intoStmt :: Stmt -> Stmt
intoStmt = \case
  SBlock block                 -> SBlock $ intoBlock block
  SIf expr stmt                -> SIf expr (intoStmt stmt)
  SIfElse expr stmt1 stmt2     -> SIfElse expr (intoStmt stmt1) (intoStmt stmt2)
  SWhile expr stmt             -> SWhile expr $ intoStmt stmt
  SDo expr stmt                -> SDo expr (intoStmt stmt)
  SForB mfi me mes s           -> addBlock $ forToWhile (SForB mfi me mes s)
  SForE vmType ident expr stmt -> SForE vmType ident expr (intoStmt stmt)
  stmt                         -> stmt

-- | creates a block around a stmt
addBlock :: [Stmt] -> Stmt
addBlock = SBlock . Block

-- | takes a for-loop, converts it into a while by
-- adding a block around it
forToWhile :: Stmt -> [Stmt]
forToWhile (SForB mfi mc mups si) =
  [ mfiToStmt mfi
  , SWhile (convForCond mc) (addExpsIntoStmt mups $ intoStmt si)]

-- | converts a maybe condition into a condition for a while-loop
convForCond :: Maybe Expr -> Expr
convForCond = fromMaybe $ ELit $ Boolean True

{-
  makes maybe exprs into SExprs and puts them
  in same sblock if there is one, otherwise if only 1 statement
  and no block, create a block around and add SExprs.
-}
addExpsIntoStmt :: Maybe [Expr] -> Stmt -> Stmt
addExpsIntoStmt = flip $ \stmt -> maybe stmt $ \exprs ->
    addBlock $ let ss = case stmt of SBlock (Block stmts) -> stmts ; _ -> [stmt]
               in  ss ++ (SExpr <$> exprs)

-- | As forInitToStmt but on Nothing, yields SEmpty.
mfiToStmt :: Maybe ForInit -> Stmt
mfiToStmt = maybe SEmpty forInitToStmt

-- | Depending on what kind of init, turn into stmts
forInitToStmt :: ForInit -> Stmt
forInitToStmt = \case
  FIVars typedVVDecl -> SVars typedVVDecl
  FIExprs exprs      -> addBlock $ SExpr <$> exprs
  HoleForInit i      -> HoleStmt i