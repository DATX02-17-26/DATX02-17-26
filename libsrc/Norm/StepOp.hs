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

-- | Normalizers for simplifying EStep / StepOp.
module Norm.StepOp (
  -- * Normalizers
    normStepFor
  , normStepSExpr
  , normStepExpr
  ) where

import Control.Lens ((%%~))

import Util.Monad (traverseJ)
import Norm.NormCS

-- TODO allocate stages. At the moment chosen arbitrarily.
stage :: Int
stage = 1

--------------------------------------------------------------------------------
-- Exported Rules:
--------------------------------------------------------------------------------

-- | Simplifies stepping expressions in the update part of basic for loops.
-- > for ( init ; cond ; i++, ... )  =>  for ( init ; cond ; i = i + 1 )
normStepFor :: NormCUR
normStepFor = makeRule' "step_op.stmt.for" [stage] execStepFor

-- | Simplifies stepping expressions in a free statement expression.
-- > i++;  =>  i = i + 1;
normStepSExpr :: NormCUR
normStepSExpr = makeRule' "step_op.stmt.sexpr" [stage] execStepSExpr

-- | Simplifies stepping expressions in any expression.
-- > x - i++   =>   x - ((i = i + 1) - 1)
-- It is important that this run AFTER
-- "step_op.stmt.sexpr" and "step_op.stmt.for"
normStepExpr :: NormCUR
normStepExpr = makeRule' "unsafe.step_op.expr" [stage + 1] execStepExpr

--------------------------------------------------------------------------------
-- step_op.stmt.for:
--------------------------------------------------------------------------------

execStepFor :: NormCUA
execStepFor = normEvery $ (sForEPost %%~) . traverse . traverseJErr $
  extStep >=> change . exprIntoExprStmts . changeStep

--------------------------------------------------------------------------------
-- step_op.stmt.sexpr:
--------------------------------------------------------------------------------

execStepSExpr :: NormCUA
execStepSExpr = normEvery $ traverseJErr $
  (^?? _SExpr) >=> extStep >=> change . exprIntoStmts . changeStep

--------------------------------------------------------------------------------
-- step_op.expr:
--------------------------------------------------------------------------------

execStepExpr :: NormCUA
execStepExpr = normEvery $ withError' $ extStep >=> change . changeStep

--------------------------------------------------------------------------------
-- Common Logic:
--------------------------------------------------------------------------------

-- | Simplifies (LValue, StepOp) into a simpler form.
-- > i++ => (i = i + 1) - 1
-- > i-- => (i = i - 1) + 1
-- > ++i => i = i + 1
-- > --i => i = i - 1
changeStep :: (LValue, StepOp) -> Expr
changeStep (lv, sop) =
  -- TODO: definition of one is not semantic preserving and unsafe.
  -- It will fail in the case of char/byte/short i = 0; i++
  -- Fix this by inferring the type of LValue.
  let one = ELit $ Int 1
      (nop, after) = case sop of
        PostInc -> (Add, flip (ENum Sub) one)
        PostDec -> (Sub, flip (ENum Add) one)
        PreInc  -> (Add, id)
        PreDec  -> (Sub, id)
  in after $ EAssign lv $ ENum nop (EVar lv) one

-- | Is a normalization semantic preserving (SP) for EStep ?
-- For example,
-- > arr[i++]
-- Has side effects and will thus not preserve semantics.
isNormSP :: LValue -> Bool
isNormSP = \case
  LVName {}     -> True
  LVArray a is  -> True -- TODO: analyze purity of constitutents.
  HoleLValue {} -> False

-- | Extracts the StepOP + LValue of an EStep if possible.
extStep :: Expr -> NormE (LValue, StepOp)
extStep = mayDecline . \case
  EStep op (EVar lv) | isNormSP lv -> pure (lv, op)
  _                                -> Nothing