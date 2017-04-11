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

module Norm.ForIndexConstLessThanEq (normForIndexCLTE) where

import Norm.NormCS
import CoreS.AST as AST

stage :: Int
stage = 1

normForIndexCLTE :: NormCUR
normForIndexCLTE = makeRule' "for_index.stmt.for_index.clte" [stage] execForIndex

--forIndex.stmt.for_index
execForIndex  :: NormCUA
execForIndex = normEvery $ \case
  x@(SForB mForInit mExpr mExprs stmt) ->
    case mExpr of
      Just (ECmp LE left (ELit (Int i)))
        -> if left `isOk` mForInit then
              change $
                SForB
                  mForInit
                  (Just (ECmp AST.LT left (ELit (Int (i+1)))))
                  mExprs
                  stmt
           else
            unique x
      _ -> unique x
  x -> unique x

(EVar (LVName (Name [Ident ident]))) `isOk` (Just (FIVars (TypedVVDecl (VMType _ (PrimT IntT)) [VarDecl (VarDId (Ident ident')) _]))) = ident == ident'
_ `isOk` _ = False
