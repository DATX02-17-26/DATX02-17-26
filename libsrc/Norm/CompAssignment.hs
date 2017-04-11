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

-- | Normalizer for transforming compound assignment in to assignment.
module Norm.CompAssignment (normCompAss) where

import Norm.NormCS

stage :: Int
stage = 5

-- | x <op>= y => x = x <op> y
normCompAss :: NormCUR
normCompAss = makeRule' "compund_assign.expr.compund_assign_to_assign" [stage]
                        execCompAss

-- | executes normalization of compund assignments
execCompAss :: NormCUA
execCompAss = normEvery $ \case
  EOAssign lv op expr -> change $ EAssign lv $ ENum op (EVar lv) expr
  x                   -> unique x
