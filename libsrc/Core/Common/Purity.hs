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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

module Core.Common.Purity where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import Control.Lens (Lens')

import Core.Common.TH

--------------------------------------------------------------------------------
-- Purity:
--------------------------------------------------------------------------------

-- | The purity of some term.
data Purity
  = Constant -- ^ Indicates that the value is known at compile time.
  | Pure     -- ^ Indicates that it is referentially transparent.
  | Impure   -- ^ Indicates that it is not referentially transparent.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

$(deriveLens [''Purity])

instance Monoid Purity where
  mempty      = Constant
  mappend x y = if x < y then y else x

-- | Determines if the term which is has this purity flag is pure or not.
-- Constant implies pure.
isPure :: Purity -> Bool
isPure = (< Impure)

-- | Class of terms with knowledge about purity.
class HasPurity x where
  -- | Lens for the purity of the term.
  purity :: Lens' x Purity
