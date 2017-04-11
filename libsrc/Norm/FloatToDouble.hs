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

-- | Normalizers for transforming floats in to doubles.
module Norm.FloatToDouble ( normFloatToDoubleVars, normFloatToDoubleRet ) where

import Norm.NormCS

-- TODO allocate stages. At the moment chosen arbitrarily.
stage :: Int
stage = 15

-- | Rule to transform float vars into double vars
-- > float x; => double x;
normFloatToDoubleVars :: NormCUR
normFloatToDoubleVars = makeRule' "unsafe.coerce_float_to_double.vardecl"
                                  [stage] execFloatToDoubleVars

-- | Rule to transform methods with float return type into double return type.
-- > public static float x(){} => public static double x(){}
normFloatToDoubleRet :: NormCUR
normFloatToDoubleRet = makeRule' "unsafe.coerce_float_to_double.method"
                                 [stage + 1] execFloatToDoubleRet

-- | Transforms float variable inits, decls and references in to doubles
execFloatToDoubleVars :: NormCUA
execFloatToDoubleVars = normEvery $ \case
  VMType t (PrimT FloatT) -> change $ VMType t (PrimT DoubleT)
  x                       -> unique x

-- | Transforms float method return types in to double return types
execFloatToDoubleRet :: NormCUA
execFloatToDoubleRet = normEvery $ \case
  MethodDecl (Just (PrimT FloatT)) n p b ->
    change $ MethodDecl (Just (PrimT DoubleT)) n p b
  x                                      -> unique x
