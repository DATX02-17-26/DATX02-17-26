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
  (LVArray a as) -> sum $ map nbrOfStatements (a:as)
  (InitExpr a) -> nbrOfStatements a
  (InitArr as) -> sum $ map nbrOfStatements as
  (ELit a) -> nbrOfStatements a
  (EVar a) -> nbrOfStatements a
  (ECast _ a) -> nbrOfStatements a
  (ECond a1 a2 a3) -> sum $ map nbrOfStatements [a1,a2,a3]
  (EAssign a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (EOAssign a1 _ a2) -> sum $ map nbrOfStatements [a1,a2]
  (ENum _ a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (ECmp _ a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (ELog _ a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (ENot a) -> nbrOfStatements a
  (EStep _ a) -> nbrOfStatements a
  (EBCompl a) -> nbrOfStatements a
  (EPlus   a) -> nbrOfStatements a
  (EMinus  a) -> nbrOfStatements a
  (EMApp _ as) -> sum $ map nbrOfStatements as
  (EArrNew  _ as _) -> sum $ map nbrOfStatements as
  (EArrNewI _ _ as) -> sum $ map nbrOfStatements as
  (ESysOut a) -> nbrOfStatements a
  (Block as) -> sum $ map nbrOfStatements as
  (SExpr a) -> nbrOfStatements a
  (SReturn a) -> nbrOfStatements a
  (SIf a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (SIfElse a1 a2 a3) -> sum $ map nbrOfStatements [a1,a2,a3]
  (SWhile a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (SDo a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (SForB ma1 ma2 mas a) ->  sum $ map nbrOfStatements $ a : maybeToList ma1 ++ maybeToList ma2 ++ (concat . maybeToList) mas
  (SForE _ _ a1 a2) -> sum $ map nbrOfStatements [a1,a2]
  (SSwitch a as) -> sum $ map nbrOfStatements (a:as)
  (SwitchBlock _ as) -> sum $ map nbrOfStatements as
  (SwitchCase a) -> nbrOfStatements a
  (FIExprs as) -> sum $ map nbrOfStatements as
  (MethodDecl _ _ _ as) -> sum $ map nbrOfStatements as
  (CompilationUnit as) -> sum $ map nbrOfStatements as
  (ClassTypeDecl a) -> nbrOfStatements a
  (ClassDecl _ a) -> nbrOfStatements a
  (ClassBody as) -> sum $ map nbrOfStatements as
  (MemberDecl a) -> nbrOfStatements a
  _ -> 0
