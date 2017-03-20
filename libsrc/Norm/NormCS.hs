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

-- | Entrypoint for normalizations for CoreS.AST.
-- This should be imported when writing normalizers.
-- It reexports CoreS.AST, Norm.NormM, NormalizationStrategies.
module Norm.NormCS (
   NormCUR
 , NormCUA
 , module RE
 ) where

import CoreS.AST               as RE
import Norm.NormM              as RE
import NormalizationStrategies as RE

-- | NormalizationRule for CompilationUnit:s.
-- This is the top level normalizer.
type NormCUR = NormalizationRule CompilationUnit

-- | Normalization arrow for CompilationUnit:s.
-- This is the top level normalizer arrow.
type NormCUA = NormArr CompilationUnit