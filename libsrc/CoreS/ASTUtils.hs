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

-- | Utilities for CoreS.AST.
module CoreS.ASTUtils (
  -- * Identifiers and Names
    singName
  -- * Types
  , isTNum
  , isTInt
  , typeFold
  , typeDimens
  , typeBase
  -- * Literals
  , litTrue
  , litFalse
  , litBoolEq
  -- * lvalues
  , singVar
  -- * Expressions, Statements, Blocks
  , allowedInSExpr
  , ternaryIntoStmts
  , logIntoStmts
  , exprIntoStmts
  , exprIntoExprStmts
  , mkSBlock
  ) where

import Data.Maybe (fromMaybe)
import Control.Monad ((>=>))

import Control.Lens ((^?), isn't, (^..))
import Data.Data.Lens (uniplate)

import CoreS.AST

--------------------------------------------------------------------------------
-- Idents & Names:
--------------------------------------------------------------------------------

-- | Constructs a Name from a single Ident.
singName :: Ident -> Name
singName = Name . pure

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

-- | Yields True if the type is primitive numeric.
isTNum :: Type -> Bool
isTNum t = fromMaybe False $ isn't _BoolT <$> (t ^? tPrim)

-- | Yields True if the type is primitive integral.
isTInt :: Type -> Bool
isTInt = (`elem` [byT, chT, shT, inT, loT])

-- Fold a type into something else recursively until it reaches a base type.
-- Tail recursive fold.
typeFold :: (b -> Type -> b) -> b -> Type -> b
typeFold f z = \case
  ArrayT t -> typeFold f (f z t) t
  t        -> z

-- | Dimensionality of a type, an array adds +1 dimensionality.
typeDimens :: Type -> Integer
typeDimens = typeFold (const . (+1)) 0

-- | Base type of type - given a base type, this is id.
typeBase :: Type -> Type
typeBase t = typeFold (flip const) t t

--------------------------------------------------------------------------------
-- Literals:
--------------------------------------------------------------------------------

-- | Yields True if the given expression is a True literal.
litTrue :: Expr -> Bool
litTrue = litBoolEq True

-- | Yields True if the given expression is a False literal.
litFalse :: Expr -> Bool
litFalse = litBoolEq False

-- | Yields True if the given expression is == the given bool literal.
litBoolEq :: Bool -> Expr -> Bool
litBoolEq eq = \case
  ELit (Boolean b) | b == eq -> True
  _                          -> False

--------------------------------------------------------------------------------
-- lvalues:
--------------------------------------------------------------------------------

-- | Constructs an LValue from a single Ident.
-- This needn't be a local variable, but could instead be a static field
-- of some static import, or in the future a static field of the same class.
singVar :: Ident -> LValue
singVar = LVName . singName

--------------------------------------------------------------------------------
-- Expressions, Statements, Blocks:
--------------------------------------------------------------------------------

-- | Determines if the given Expr is allowed in an SExpr
-- according to JLS § 14.8. Expression Statements
allowedInSExpr :: Expr -> Bool
allowedInSExpr = \case
  EAssign  {} -> True
  EOAssign {} -> True
  EStep    {} -> True
  EMApp    {} -> True
  EInstNew {} -> True
  ESysOut  {} -> True
  _           -> False

-- | Convert a ternary expression, expanded, into a list of statements.
-- This is a special case since only the side-effects of one branch will
-- occur depending on the conditional expression.
ternaryIntoStmts :: Expr -> Expr -> Expr -> [Stmt]
ternaryIntoStmts c ei ee =
  let sis = exprIntoStmts ei
      ses = exprIntoStmts ee
  in  if null sis && null ses
      then exprIntoStmts c
      else pure $ SIfElse c (mkSBlock sis) (mkSBlock ses)

-- | Convert a logical expression, expanded, into a list of statements.
-- This is a special case due to the short-circuiting natureo of && and ||.
logIntoStmts :: Expr -> Expr -> LogOp -> [Stmt]
logIntoStmts l r o =
  let srs = exprIntoStmts r
      fop = case o of LAnd -> id ; LOr -> ENot ;
  in  if null srs then exprIntoStmts l else pure $ SIf (fop l) (mkSBlock srs)

-- | Split an expression into parts allowed as statements.
-- Such statements may or may not have side-effects.
exprIntoStmts :: Expr -> [Stmt]
exprIntoStmts = \case
  -- Already allowed as a Stmt, so no-op:
  e | allowedInSExpr e -> [SExpr e]
  -- Ternaries require special treatment as they correspond to if else:
  ECond c ei ee        -> ternaryIntoStmts c ei ee
  -- Logical operators && and || are short circuiting:
  ELog o l r           -> logIntoStmts l r o
  -- Otherwise, collect all subexprs left-to-right, convert those & merge:
  e                    -> e ^.. uniplate >>= exprIntoStmts

-- | Split an expression into parts that are allowed as expression statements
-- as specified by allowedInSExpr.
exprIntoExprStmts :: Expr -> [Expr]
exprIntoExprStmts = exprIntoStmts >=> (^.. _SExpr)

-- | Construct a statement block out of a list of statements.
mkSBlock :: [Stmt] -> Stmt
mkSBlock = SBlock . Block