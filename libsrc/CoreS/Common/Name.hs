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

-- | Naming + identifier related parts of AST + utilites.
module CoreS.Common.Name where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Util.TH (deriveLens)

--------------------------------------------------------------------------------
-- Names and identifiers:
--------------------------------------------------------------------------------

-- | Ident: Identifier of a variable, class, method, etc.
data Ident = Ident
  { _idId   :: String    -- ^ The actual name of the identifier.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Name: Qualified identifier.
data Name = Name
  { _nmIds  :: [Ident] -- ^ Identifier parts of the name.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | VarDeclId: identifier of variable declarations.
data VarDeclId
  = VarDId {
      _vdiIdent :: Ident   -- ^ Identifier of the variable.
    }
  | VarDArr {
      _vdiIdent :: Ident   -- ^ Identifier of the variable.
    , _vdiDimen :: Integer -- ^ Dimensionality of the variable.
    }
  | HoleVarDeclId {
      _vdiHole   :: Int    -- ^ TODO DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Constructs a Name from a single Ident.
singName :: Ident -> Name
singName = Name . pure

--------------------------------------------------------------------------------
-- NFData:
--------------------------------------------------------------------------------

instance NFData Ident
instance NFData Name
instance NFData VarDeclId

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [''Ident, ''Name, ''VarDeclId])