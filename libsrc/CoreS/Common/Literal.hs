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

-- | Literal related parts of AST.
module CoreS.Common.Literal where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Util.TH (deriveLens)

--------------------------------------------------------------------------------
-- Literals:
--------------------------------------------------------------------------------

-- | Literal values.
data Literal
  = Int {
      _litI  :: Integer -- ^ Literal integer, type is IntT, example: "1".
    }
  | Word {
      _litI  :: Integer -- ^ Literal word, type is LongT, example: "1L".
    }
  | Float {
      _litD  :: Double  -- ^ Literal float, type is FloatT, example: "0.1f".
    }
  | Double {
      _litD  :: Double  -- ^ Literal double, type is DoubleT, example: "0.0".
    }
  | Boolean {
      _litB  :: Bool    -- ^ Literal boolean, type is BoolT, example: "true".
    }
  | Char {
      _litC  :: Char    -- ^ Literal char, type is CharT, example: "'a'".
    }
  | String {
      _litS  :: String  -- ^ Literal String, type is StringT, example: "\"A\"".
    }
  | Null                -- ^ Literal null, type is NullT, example: "null".
  | HoleLiteral {
      _litHole :: Int   -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- NFData:
--------------------------------------------------------------------------------

instance NFData Literal

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [''Literal])