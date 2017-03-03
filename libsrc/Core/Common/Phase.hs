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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- | Class of phase indexes.
module Core.Common.Phase (
    PhaseIndex, PhaseIndex2
  , phaseId, phaseId21, phaseId22
  , PhaseId (..)
  ) where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Phase Indexing:
--------------------------------------------------------------------------------

-- | The PhaseId of a phase-index.
newtype PhaseId = PhaseId String
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Class of types that model a phase index of types with one type parameter.
class PhaseIndex pi where
  -- | Yields the PhaseId for a given term phase-indexed by pi.
  phaseId :: f pi -> PhaseId

-- | Class of types that model a phase index of types with two type parameters.
class PhaseIndex2 pi where
  -- | Yields the PhaseId for a given term phase-indexed by pi in first pos.
  phaseId21 :: f pi a -> PhaseId

  -- | Yields the PhaseId for a given term phase-indexed by pi in second pos.
  phaseId22 :: f a pi -> PhaseId