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

{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase #-}

-- Useful TemplateHaskell utilities.
module Util.TH (
  -- * Various derivators.
    stdDerive
  , stdDerives
  , deriveLens
  , recordIx
  , recordIxs
  ) where

import Data.List (uncons)
import Control.Monad (zipWithM)
import Control.Lens (makeLenses, makePrisms)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Util.Monad (traverseS)

-- | Standalone derive, given:
-- + constraint constructor :: (* -> Constraint) -> * -> Constraint,
-- + class to derive,
-- + type to derive for.
stdDerive :: Name -> Name -> Name -> DecQ
stdDerive constr clazz typ = do
  p <- varT <$> newName "p"
  let tconstr = pure $ ConT constr
  let tclazz  = pure $ ConT clazz
  let ttyp    = pure $ ConT typ
  standaloneDerivD (cxt [[t|$tconstr $tclazz $p|]])
                   [t| $tclazz ($ttyp $p) |]

-- | Standalone derive:
-- + given constraint constr,
-- + given set of classes,
-- + given the types,
-- all the classes for all the types.
stdDerives :: Name -> [Name] -> [Name] -> DecsQ
stdDerives constr clazzes types =
  sequence $ stdDerive <$> [constr] <*> clazzes <*> types

-- | deriveLens: derive lens & prisms for a "set" of types.
deriveLens :: Traversable t => t Name -> DecsQ
deriveLens = fmap concat . mapM (\n -> (++) <$> makeLenses n <*> makePrisms n)

-- | See 'recordIx'.
recordIxs :: Traversable t => t Name -> DecsQ
recordIxs = traverseS recordIx

-- | Derive the relative field indices, starting from 0,
-- of all constructors in the given
-- record newtype or datatype.
recordIx :: Name -> DecsQ
recordIx typeN = do
  var  <- reify typeN
  dec  <- case var of
    TyConI dec -> pure dec
    _ -> fail "recordIx: Expected type constructor name"
  cons <- case dec of
    DataD    _ _ _ _ cons _ -> pure cons
    NewtypeD _ _ _ _ con  _ -> pure [con]
    _ -> fail "recordIx: Expected data or newtype type-constructor"
  flip traverseS cons $ \case
    RecC _ vbt -> zipWithM recCtorIx [0..] $ ixName <$> vbt
    x          -> pure []

recCtorIx :: Integer -> Name -> DecQ
recCtorIx ix fname = pure $
  ValD (VarP fname) (NormalB (LitE (IntegerL ix))) []

ixName :: VarBangType -> Name
ixName (n, _, _) = mkName . (++ "Ix") . removeUS . nameBase $ n

removeUS :: String -> String
removeUS = \case
  []      -> []
  ('_':t) -> t
  xs      -> xs