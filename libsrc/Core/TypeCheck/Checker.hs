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

{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

{-
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE GADTs, DataKinds, PolyKinds, KindSignatures, TypeFamilies
  , LambdaCase, TypeOperators, TemplateHaskell, TypeSynonymInstances
  , FlexibleInstances #-}
-}

module Core.TypeCheck.Checker where

import qualified Data.Map as M

import Data.List (uncons)
import Control.Arrow (second, (***))
import Control.Monad.State
import Control.Monad.Except

import Control.Lens ((%%=), (.=), (+~), use)

import Core.Common.TH
import Core.Common.AST
import Core.Start.AST
import Core.TypeCheck.AST

type Context  = M.Map String LVExt
type Contexts = [Context]

data TCEnv = TCEnv
  { _contexts :: Contexts -- Stack of contexts for variables.
  } deriving (Eq, Ord, Show)

$(deriveLens [''TCEnv])

initTCEnv :: TCEnv
initTCEnv = TCEnv [M.empty]

newtype TCComp a = TCComp
  { runTTComp :: StateT TCEnv (Except ()) a
  } deriving (Functor, Applicative, Monad, MonadFix,
              MonadState TCEnv, MonadError ())

u = undefined

err :: TCComp a
err = throwError ()

-- NOTE: These functions are somewhat borrowed from previous work / Mazdak
-- From a course in Compiler Construction...

-- | 'mfindU': specializes 'mfind' for lists while also allowing an update to
-- the place the element was considered found. The updated list is yielded
-- alongside the potenially found transformed element as a pair.
mfindU :: (a -> Maybe (b, a)) -> [a] -> (Maybe b, [a])
mfindU tp = maybe (Nothing, []) f . uncons
    where f (a, as) = maybe (second (a:) $ mfindU tp as) (Just *** (:as)) (tp a)

merr :: Maybe a -> TCComp a
merr = maybe err pure

loadVar, writeVar :: String -> TCComp LVExt
loadVar  key = contexts %%= mfindU (ctxLookup readsInc  key) >>= merr
writeVar key = contexts %%= mfindU (ctxLookup writesInc key) >>= merr

readsInc, writesInc :: LVExt -> LVExt
readsInc  = (leVar . veReads)  +~ 1
writesInc = (leVar . veWrites) +~ 1

ctxLookup :: (LVExt -> LVExt) -> String -> Context -> Maybe (LVExt, Context)
ctxLookup upd key ctx = case M.updateLookupWithKey (const $ Just . upd) key ctx of
    (Just var, ctx') -> Just (var, ctx')
    (Nothing , _   ) -> Nothing

extendVar :: LVExt -> String -> TCComp ()
extendVar set key = do
    (c : ctxs) <- use contexts
    maybe (contexts .= M.insert key set c : ctxs)
          (const err)
          (M.lookup key c)

coercesTo, castableTo :: Type -> Type -> TCComp Bool
--coercesTo (PrimT x) (PrimT y) = 
coercesTo x y | x == y    = pure True
              | otherwise = pure False

castableTo x y = coercesTo x y

inferExpr :: SExpr -> TCComp (TcExpr, ExprExt)
inferExpr expr = case expr of
  EExpr    {} -> u
  ELit     {} -> u
  EVar     {} -> u
  ECast    {} -> u
  ECond    {} -> u
  EAssign  {} -> u
  EOAssign {} -> u
  ENum     {} -> u
  ECmp     {} -> u
  ELog     {} -> u
  ENot     {} -> u
  EStep    {} -> u
  EBCompl  {} -> u
  EPlus    {} -> u
  EMinus   {} -> u
  EMApp    {} -> u
  EArrNew  {} -> u
  EArrNewI {} -> u
  ESysOut  {} -> u