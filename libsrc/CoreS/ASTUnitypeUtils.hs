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

module CoreS.ASTUnitypeUtils where

import CoreS.ASTUnitype
import Data.Maybe (maybeToList)

--   (`dependsOn y x` is `True` if `y` depends on `x`
--
-- if `dependsOn y x == False` then `y; x;` and `x; y;`
-- should be semantically identical
dependsOn :: AST -> AST -> Bool
dependsOn SEmpty _ = False
dependsOn _ SEmpty = False
dependsOn (MethodDecl _ _ _ _) (MethodDecl _ _ _ _) = False
dependsOn (MemberDecl _) (MemberDecl _) = False
dependsOn y x = True

nbrOfStatements :: AST -> Int
nbrOfStatements a = 1 + case a of
  LVArray a as          -> sm $ a:as
  InitExpr a            -> nbrOfStatements a
  InitArr as            -> sm as
  ELit a                -> nbrOfStatements a
  EVar a                -> nbrOfStatements a
  ECast _ a             -> nbrOfStatements a
  ECond a b c           -> sm [a, b, c]
  EAssign a b           -> sm [a, b]
  EOAssign a _ b        -> sm [a, b]
  ENum _ a b            -> sm [a, b]
  ECmp _ a b            -> sm [a, b]
  ELog _ a b            -> sm [a, b]
  ENot    a             -> nbrOfStatements a
  EStep _ a             -> nbrOfStatements a
  EBCompl a             -> nbrOfStatements a
  EPlus   a             -> nbrOfStatements a
  EMinus  a             -> nbrOfStatements a
  EMApp _ as            -> sm as
  EArrNew  _ as _       -> sm as
  EArrNewI _ _ as       -> sm as
  ESysOut a             -> nbrOfStatements a
  Block as              -> sm as
  SExpr a               -> nbrOfStatements a
  SReturn a             -> nbrOfStatements a
  SIf a b               -> sm [a, b]
  SIfElse a b c         -> sm [a, b, c]
  SWhile a b            -> sm [a, b]
  SDo a b               -> sm [a, b]
  SForB mas mbs mcs d   -> sm $ d : maybeToList mas ++
                                    maybeToList mbs ++
                                    (concat . maybeToList) mcs
  SForE _ _ a b         -> sm [a, b]
  SSwitch a bs          -> sm $ a:bs
  SwitchBlock _ as      -> sm as
  SwitchCase a          -> nbrOfStatements a
  FIExprs as            -> sm as
  MethodDecl _ _ _ as   -> sm as
  CompilationUnit is as -> sm $ is ++ as
  ClassTypeDecl a       -> nbrOfStatements a
  ClassDecl _ a         -> nbrOfStatements a
  ClassBody as          -> sm as
  MemberDecl a          -> nbrOfStatements a
  _                     -> 0
  where sm x = sum $ map nbrOfStatements x