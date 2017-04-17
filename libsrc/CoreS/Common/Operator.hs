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

-- | Operator related parts of AST.
module CoreS.Common.Operator where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Util.TH (deriveLens)

--------------------------------------------------------------------------------
-- Operators:
--------------------------------------------------------------------------------

-- NumOp: Numeric operators, both in assignment and as binary operators.
data NumOp
  = Add     -- ^ Numeric operator for Addition.
  | Sub     -- ^ Numeric operator for Subtraction.
  | Mul     -- ^ Numeric operator for Multiplication.
  | Div     -- ^ Numeric operator for Division.
  | Rem     -- ^ Numeric operator for Remainder (not modulo).
  | LShift  -- ^ Numeric operator for Left shift.
  | RShift  -- ^ Numeric operator for Right shift.
  | RRShift -- ^ Numeric operator for Unsigned right shift.
  | And     -- ^ Numeric operator for Bitwise And.
  | Xor     -- ^ Numeric operator for Bitwise Xor.
  | Or      -- ^ Numeric operator for Bitwise Or.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- CmpOp: (binary) comparison operators, result is always boolean typed.
data CmpOp
  = EQ -- ^ Comparison operator for Equality (==).
  | NE -- ^ Comparison operator for Inequality (!=).
  | LT -- ^ Comparison operator for Less than (<).
  | GT -- ^ Comparison operator for Greater than (>).
  | LE -- ^ Comparison operator for Less than / Equal to (<=).
  | GE -- ^ Comparison operator for Greater than / Equal to (>=).
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- LogOp: (binary) logical operators, operands and results are boolean typed.
data LogOp
  = LAnd -- ^ Logical conjunction \land (LaTeX) operator.
  | LOr  -- ^ Logical disjunction \lor  (LaTeX) operator.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | StepOp: Unary stepping operators, operand must be of a numeric type.
data StepOp
  = PostInc -- ^ Unary operator for Post Incrementation (i++).
  | PostDec -- ^ Unary operator for Post Decrementation (i--).
  | PreInc  -- ^ Unary operator for Pre Incrementation (++i).
  | PreDec  -- ^ Unary operator for Pre Incrementation (--i).
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- NFData:
--------------------------------------------------------------------------------

instance NFData NumOp
instance NFData CmpOp
instance NFData LogOp
instance NFData StepOp

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [''NumOp, ''CmpOp, ''LogOp, ''StepOp])