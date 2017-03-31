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
import qualified CoreS.AST as C
import Data.Maybe (maybeToList)
import Data.Set (fromList, union, intersection)

--   (`dependsOn y x` is `True` if `y` depends on `x`
--
-- if `dependsOn y x == False` then `y; x;` and `x; y;`
-- should be semantically identical
dependsOn :: AST -> AST -> Bool
dependsOn SEmpty _ = False
dependsOn _ SEmpty = False
{-dependsOn a1 a2 =
  (not $ null $ (fromList $ changesIds a1) `union` ((fromList $ changesIds a2) `intersection` (fromList $ changesIds a2))) ||
  (not $ null $ (fromList $ usesIds a1) `intersection` (fromList $ changesIds a2))-}

usesIds :: AST -> [C.Ident]
usesIds = \case
  (LVName id)               -> [id]
  (LVArray a as)            -> um $ a:as
  (InitExpr a)              -> usesIds a
  (InitArr  as)             -> um as
  (ELit a)                  -> usesIds a
  (EVar a)                  -> usesIds a
  (ECast _ a )              -> usesIds a
  (ECond a1 a2 a3)          -> um [a1,a2,a3]
  (EAssign a1 a2)           -> um [a1,a2]
  (EOAssign a1 _ a2)        -> um [a1,a2]
  (ENum _ a1 a2)            -> um [a1,a2]
  (ECmp _ a1 a2)            -> um [a1,a2]
  (ELog _ a1 a2)            -> um [a1,a2]
  (ENot a)                  -> usesIds a
  (EStep _ a)               -> usesIds a
  (EBCompl a)               -> usesIds a
  (EPlus a)                 -> usesIds a
  (EMinus a)                -> usesIds a
  (EMApp n as)              -> (C._nmIds n) ++ (um as) -- should the identifier parts of the name be in here?
  (EArrNew  _ as _)         -> um as
  (EArrNewI _ _ as)         -> um as
  (ESysOut a)               -> usesIds a
  (Block as)                -> um as
  (SExpr a)                 -> usesIds a
  (SVars d)                 -> map (C._vdiIdent . C._vdVDI) (C._tvdVDecls d) -- must think about list of maybe VarInits as well
  (SReturn a)               -> usesIds a
  (SIf a1 a2)               -> um [a1,a2]
  (SIfElse a1 a2 a3)        -> um [a1,a2,a3]
  (SWhile a1 a2)            -> um [a1,a2]
  (SDo a1 a2)               -> um [a1,a2]
  (SForB ma1 ma2 mas a)     -> um $ a : maybeToList ma1 ++
                                    maybeToList ma2 ++
                                    (concat . maybeToList) mas
  (SForE _ id a1 a2)        -> id : um [a1,a2]
  (SSwitch a as)            -> um $ a:as
  (SwitchBlock l as)        -> undefined -- switchLabels contains expressions, which may include names, maybe just find the expression and convert to unitype?
  (SwitchCase a)            -> usesIds a
  (FIVars d)                -> map (C._vdiIdent . C._vdVDI) (C._tvdVDecls d) -- must think about list of maybe VarInits as well
  (FIExprs as)              -> um as
  --(MethodDecl _ id fps as)  -> id : ((map (C._vdiIdent . C._fpVDI) fps) ++ um as)
  --(CompilationUnit as1 as2) -> um $ as1 ++ as2
  (ClassTypeDecl a)         -> usesIds a
  --(ClassDecl id a)          -> id : usesIds a
  (ClassBody as)            -> um as
  (MemberDecl a)            -> usesIds a
  _                         -> []
  where um x = concat $ map usesIds x

changesIds :: AST -> [C.Ident]
changesIds = \case
  (LVName id)               -> [id] -- find everything that could acctually reach this, should those be replaced by usesId?
  (LVArray a as)            -> (usesIds a) ++ (cm $ as) -- should this be uses? also see above
  (InitExpr a)              -> changesIds a
  (InitArr  as)             -> cm as
  (ECast _ a )              -> changesIds a
  (ECond a1 a2 a3)          -> cm [a1,a2,a3]
  (EAssign a1 a2)           -> cm [a1,a2] -- reaches LValue
  (EOAssign a1 _ a2)        -> cm [a1,a2] -- reaches LValue
  (ENum _ a1 a2)            -> cm [a1,a2]
  (ECmp _ a1 a2)            -> cm [a1,a2]
  (ELog _ a1 a2)            -> cm [a1,a2]
  (ENot a)                  -> changesIds a
  (EStep _ a)               -> changesIds a
  (EBCompl a)               -> changesIds a
  (EPlus a)                 -> changesIds a
  (EMinus a)                -> changesIds a
  (EMApp _ as)              -> cm as
  (EArrNew  _ as _)         -> cm as
  (EArrNewI _ _ as)         -> cm as
  (ESysOut a)               -> changesIds a
  (Block as)                -> cm as
  (SExpr a)                 -> changesIds a
  (SVars d)                 -> C._vdiIdent . C._vdVDI <$> C._tvdVDecls d
  (SReturn a)               -> changesIds a
  (SIf a1 a2)               -> cm [a1,a2]
  (SIfElse a1 a2 a3)        -> cm [a1,a2,a3]
  (SWhile a1 a2)            -> cm [a1,a2]
  (SDo a1 a2)               -> cm [a1,a2]
  (SForB ma1 ma2 mas a)     -> cm $ a : maybeToList ma1 ++
                                    maybeToList ma2 ++
                                    (concat . maybeToList) mas
  (SForE _ id a1 a2)        -> id : cm [a1,a2]
  (SSwitch a as)            -> cm $ a:as
  (SwitchBlock l as)        -> undefined -- switchLabels contains expressions, which may include names, maybe just find the expression and convert to unitype?
  (SwitchCase a)            -> changesIds a
  (FIVars d)                -> map (C._vdiIdent . C._vdVDI) (C._tvdVDecls d)
  (FIExprs as)              -> cm as
  --(MethodDecl _ id fps as)  -> id : ((map (C._vdiIdent . C._fpVDI) fps) ++ cm as)
  --(CompilationUnit as1 as2) -> cm $ as1 ++ as2
  (ClassTypeDecl a)         -> changesIds a
  --(ClassDecl id a)          -> id : changesIds a
  (ClassBody as)            -> cm as
  (MemberDecl a)            -> changesIds a
  _                         -> []
  where cm x = concat $ map changesIds x

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
