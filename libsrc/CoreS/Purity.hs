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

{-# LANGUAGE
    DeriveDataTypeable, DeriveGeneric, TemplateHaskell, LambdaCase
  #-}

module CoreS.Purity where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import Data.Functor.Identity

import Util.TH (deriveLens)
import Util.Tree
import CoreS.AST

import Data.Monoid

import Util.Monad
import Control.Monad
import Data.Tuple
import CoreS.ASTUtils
import Data.Data.Lens
import Util.Debug
import Control.Lens ((^..))

--------------------------------------------------------------------------------
-- Data types:
--------------------------------------------------------------------------------

data Purity
  = PConst    -- ^ A constant literal or something only made up of those.
  | PPure     -- ^ A pure expr or something only made up of those.
  | PInternSF -- ^ An internally side-effectful expr or made up of those.
  | PGlobalSF -- ^ A globally side-effectful expr or made up of those.
  | PUnObsSF  -- ^ An unobservable side-effectful expr or made up of those.
  | PUnknown  -- ^ Something with unknown purity. This is the worst.
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Typeable, Data, Generic)

deriveLens [''Purity]

instance Monoid Purity where
  mempty      = PConst
  mappend x y = if y > x then y else x

--------------------------------------------------------------------------------
-- Purity decoration:
--------------------------------------------------------------------------------

type MP  = Maybe Purity
type TMP = Tree MP

padding :: Int -> Forest MP
padding n = replicate n $ pure Nothing

padL :: Int -> TMP -> TMP
padL n t = t { subForest = padding n ++ subForest t }

padR :: Int -> TMP -> TMP
padR n t = t { subForest = subForest t ++ padding n }



type PComp a = Identity a

class DecPurity t where
  decPurity :: t -> PComp TMP

u = undefined


-- | Decorates the purity of a CompilationUnit:
decoratePurity :: CompilationUnit -> TMP
decoratePurity = u

dec :: Purity -> Identity TMP
dec = pure . pure . pure

decList :: DecPurity t => [t] -> PComp TMP
decList = traverse decPurity
      >=> fkeep (pure . mconcat . fmap rootLabel)
      >=> pure . uncurry Node . swap

decSet :: DecPurity t => MP -> [t] -> PComp TMP
decSet ph = fmap (Node ph) . traverse decPurity

decSetP :: DecPurity t => Purity -> [t] -> PComp TMP
decSetP = decSet . pure

instance DecPurity Expr where
  decPurity = \case
    ELit      _ -> dec PConst
    EVar     {} -> u
    EAssign  {} -> u
    EOAssign {} -> u
    EStep  _ e  -> padL 1 <$> decSetP PInternSF [e]
    EMApp  n es -> padL 1 <$> decSetP PUnknown es
    ESysOut  e  -> decSetP PUnObsSF [e]
    HoleExpr _  -> dec PUnknown

    ECast  _ e  -> padL 1 <$> decList [e]
    ENum  _ l r -> padL 1 <$> decList [l, r]
    ECmp  _ l r -> padL 1 <$> decList [l, r]
    -- NOTE: For ECond + ELog,
    -- Laziness isn't taken into consideration.
    -- Elminiate before instead.
    ECond c l r -> decList [c, l, r]
    ELog  _ l r -> padL 1 <$> decList [l, r]

    ENot     e  -> decList [e]
    EBCompl  e  -> decList [e]
    EPlus    e  -> decList [e]
    EMinus   e  -> decList [e]
    EInstNew {} -> u
    EArrNew _ es _ -> padR 1 . padL 1 <$> decList es
    EArrNewI {} -> u
{-
-}