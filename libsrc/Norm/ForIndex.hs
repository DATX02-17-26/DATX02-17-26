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
import Data.Maybe

stage :: Int
stage = 1

normForIndex :: NormCUR
normForIndex = makeRule' "for_index.stmt.for_index" [stage] execForIndex

--forIndex.stmt.for_index
execForIndex  :: NormCUA
execForIndex = normEvery $ \case
  SForB mForInit mExpr mExprs stmt ->
    let
      ident = isValidForInit mForInit
      cmpOp = isValidCmpOp mExpr
      e = isValidInc mExprs
    in
      if (isJust ident)
          && (isJust cmpOp)
          && (isJust e)
          && (checkPlus $ fromJust e)
        then
          change $ changeFor
                      (fromJust ident)
                      (fromJust cmpOp)
                      (getLeft mExpr)
                      (getRight mExpr)
                      stmt
        else
          unique $ SForB mForInit mExpr mExprs stmt
  x -> unique x

isValidForInit :: Maybe ForInit -> Maybe Ident
isValidForInit mForInit =  case mForInit of
  Just (FIVars (TypedVVDecl (VMType VMNormal (PrimT IntT)) (v:[]))) ->
    checkI v
  _ -> Nothing

isValidCmpOp :: Maybe Expr -> Maybe CmpOp
isValidCmpOp mExpr = case mExpr of
  Just (ECmp LE left right) -> Just AST.LT
  Just (ECmp GE left right) -> Just AST.GT
  _ -> Nothing

isValidInc :: Maybe [Expr] -> Maybe Expr
isValidInc mExprs = case mExprs of
  Just (e:[]) -> Just e
  _ -> Nothing

getLeft mExpr = case mExpr of
 Just (ECmp _ left right) -> left

getRight  mExpr = case mExpr of
  Just (ECmp _ left right) -> right

changeFor :: Ident -> CmpOp -> Expr -> Expr -> Stmt -> Stmt
changeFor i op left right stmt = SForB
  (Just (FIVars (TypedVVDecl (VMType VMNormal (PrimT IntT)) [changeI i])))
  (Just (ECmp op left right))
  (Just [makePlus i])
  stmt

changeI :: Ident -> VarDecl
changeI ident = (VarDecl (VarDId ident) (Just (InitExpr (ELit (Int 0)))))

makePlus :: Ident -> Expr
makePlus ident = EStep PostInc (EVar (LVName (Name (ident:[]))))

checkI :: VarDecl -> Maybe Ident
checkI v = case v of
  (VarDecl (VarDId ident) (Just (InitExpr (ELit (Int 1))))) -> Just ident
  _ -> Nothing

checkPlus:: Expr -> Bool
checkPlus e = case e of
  EStep PostInc i -> True
  EAssign (LVName (Name (ident:[]))) (ENum Add expr1 expr2) -> True
  _ -> False
