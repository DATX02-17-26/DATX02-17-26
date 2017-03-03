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

-- NOTE: Some of these functions are somewhat borrowed from
-- previous work from a course in Compiler Construction... / Mazdak

-- | 'mfindU': specializes 'mfind' for lists while also allowing an update to
-- the place the element was considered found. The updated list is yielded
-- alongside the potenially found transformed element as a pair.
mfindU :: (a -> Maybe (b, a)) -> [a] -> (Maybe b, [a])
mfindU tp = maybe (Nothing, []) f . uncons
    where f (a, as) = maybe (second (a:) $ mfindU tp as) (Just *** (:as)) (tp a)

errM :: Maybe a -> TCComp a
errM = maybe err pure

justM :: TCComp (Maybe a) -> TCComp a
justM = (>>= errM)

getVar :: (LVExt -> LVExt) -> String -> TCComp LVExt
getVar upd key = justM $ contexts %%= mfindU (ctxLookup upd  key)

loadVar, writeVar :: String -> TCComp LVExt
loadVar  = getVar $ (leVar . veReads)  +~ 1
writeVar = getVar $ (leVar . veWrites) +~ 1

ctxLookup :: (LVExt -> LVExt) -> String -> Context -> Maybe (LVExt, Context)
ctxLookup upd k ctx = case M.updateLookupWithKey (const $ Just . upd) k ctx of
    (Just var, ctx') -> Just (var, ctx')
    (Nothing , _   ) -> Nothing

extendVar :: LVExt -> String -> TCComp ()
extendVar set key = do
    (c : ctxs) <- use contexts
    maybe (contexts .= M.insert key set c : ctxs)
          (const err)
          (M.lookup key c)

--------------------------------------------------------------------------------
-- Type conversions:
--------------------------------------------------------------------------------

type TConv = Type -> Type -> Bool

idConv :: TConv
idConv = (==)

widePrimConv :: PrimType -> PrimType -> Bool
widePrimConv s t = case s of
  ByteT  | t `elem` [ShortT, IntT, LongT, FloatT, DoubleT] -> True
  ShortT | t `elem` [IntT, LongT, FloatT, DoubleT]         -> True
  CharT  | t `elem` [IntT, LongT, FloatT, DoubleT]         -> True
  IntT   | t `elem` [LongT, FloatT, DoubleT]               -> True
  LongT  | t `elem` [FloatT, DoubleT]                      -> True
  FloatT | t `elem` [DoubleT]                              -> True
  _                                                        -> False

narrPrimConv :: PrimType -> PrimType -> Bool
narrPrimConv s t = case s of
  ShortT  | t `elem` [ByteT, CharT]                              -> True
  CharT   | t `elem` [ByteT, ShortT]                             -> True
  IntT    | t `elem` [ByteT, ShortT, CharT]                      -> True
  LongT   | t `elem` [ByteT, ShortT, CharT, IntT]                -> True
  FloatT  | t `elem` [ByteT, ShortT, CharT, IntT, LongT]         -> True
  DoubleT | t `elem` [ByteT, ShortT, CharT, IntT, LongT, FloatT] -> True
  _                                                              -> False

widePConv :: TConv
widePConv s t = case (s, t) of
  (PrimT s', PrimT t') -> widePrimConv s' t'
  _                    -> False

narrPConv :: TConv
narrPConv s t = case (s, t) of
  (PrimT s', PrimT t') -> narrPrimConv s' t'
  _                    -> False

wiNaPConv :: TConv
wiNaPConv s t = case (s, t) of
  (PrimT ByteT, PrimT CharT) -> True
  _                          -> False

wideRConv :: TConv
wideRConv s t = False -- TODO: tier 1

narrRConv :: TConv
narrRConv s t = False -- TODO: tier 1

boxConv :: TConv
boxConv s t = False -- TODO: tier 1

unBoxConv :: TConv
unBoxConv s t = False -- TODO: tier 1

nullConv :: TConv
nullConv s t = case s of
  NullT -> case t of
    ArrayT _ -> True
    StringT  -> True
    _        -> False
  _          -> False

anyConv :: (Foldable t, Functor t) => t TConv -> TConv
anyConv convs = \s t -> or $ ($ s) . ($ t) <$> convs

assConv :: TConv
assConv = anyConv [idConv, widePConv, wideRConv, boxConv, unBoxConv, nullConv]

appConv :: Type -> Type -> Bool
appConv = anyConv [idConv, widePConv, boxConv, unBoxConv, nullConv]


-- | Determines if the first type can be coerced to the second.
coercesTo :: Type -> Type -> TCComp Bool
-- coercesTo (PrimT x) (PrimT y) = u
coercesTo x y | x == y    = pure True
              | otherwise = pure False

castableTo :: Type -> Type -> TCComp Bool
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