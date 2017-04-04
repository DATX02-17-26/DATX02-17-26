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

module Norm.ForIndex where

import Norm.NormCS
import CoreS.AST as AST

stage :: Int
stage = 1

normDoWToWhile :: NormCUR
normDoWToWhile = makeRule' "forIndex.stmt.for_index" [stage] execForIndex

--forIndex.stmt.for_index
execForIndex  :: NormCUA
execForIndex = normEvery $ \case
  SForB mForInit mExpr mExprs stmt -> case mForInit of
     Just (FIVars (TypedVVDecl (VMType VMNormal (PrimT IntT)) (v:[]))) -> case checkI v of
       Just ident -> case mExpr of
          Just (ECmp LE left right) -> case mExprs of
            Just (e:[]) -> if checkPlus e then change $ changeFor AST.LT ident stmt left right  else
              unique $ SForB mForInit mExpr mExprs stmt
          Just (ECmp GE left right) -> case mExprs of
            Just (e:[]) -> if checkPlus e then change $ changeFor AST.GT ident stmt left right  else
              unique $ SForB mForInit mExpr mExprs stmt
       _ -> unique $ SForB mForInit mExpr mExprs stmt
     _ -> unique $ SForB mForInit mExpr mExprs stmt
  x -> unique x

changeFor op i stmt left right = SForB
  (Just (FIVars (TypedVVDecl (VMType VMNormal (PrimT IntT)) [changeI i])))
  (Just (ECmp op left right))
  (Just [makePlus i])
  stmt

changeI ident = (VarDecl (VarDId ident) (Just (InitExpr (ELit (Int 0)))))

makePlus ident = EStep PostInc (EVar (LVName (Name (ident:[]))))

checkI :: VarDecl -> Maybe Ident
checkI v = case v of
  (VarDecl (VarDId ident) (Just (InitExpr (ELit (Int 1))))) -> Just ident
  _ -> Nothing

checkLitI :: Ident -> Expr -> Bool
checkLitI i e = case getIdent e of
  Just ident -> i == ident
  _ -> False

getIdent :: Expr -> Maybe Ident
getIdent e = case e of
  EVar (LVName (Name (ident:[]))) -> Just ident
  _ -> Nothing

checkPlus:: Expr -> Bool
checkPlus e = case e of
  EStep PostInc i -> True
  EAssign (LVName (Name (ident:[]))) (ENum Add expr1 expr2) -> True
  _ -> False
