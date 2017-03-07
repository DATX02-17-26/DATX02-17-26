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

{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, LambdaCase
  , RecordWildCards #-}

module Core.TypeCheck.Checker where

import Prelude hiding (EQ, LT, GT)

import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except

import Data.Monoid ((<>))
--import Data.Maybe (fromMaybe)
--import Data.Foldable (find)
import Util.List

import Control.Lens ((%%=), (.=), (+~), use, (^?))

import Core.Common.History
import Core.Common.TH
import Core.Common.AST
import Core.Common.Type
import Core.Common.Purity
import Core.Common.Literal
import Core.Start.AST
import Core.TypeCheck.AST

u = undefined

--------------------------------------------------------------------------------
-- Environment:
--------------------------------------------------------------------------------

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

err :: TCComp a
err = throwError ()

lM :: Maybe a -> TCComp a
lM = maybe err pure

justM :: TCComp (Maybe a) -> TCComp a
justM = (>>= lM)

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
-- Type inference:
--------------------------------------------------------------------------------

inferLit :: SExpr -> TCComp TcExpr
inferLit e = let l = _eLit e in pure $ ELit NoH (EExt Constant $ litType l) l

geType :: TcExpr -> TCComp Type
geType = lM . (^? eType)

gePurity :: HasPurity x => x -> TCComp Purity
gePurity = lM . (^? purity)

inferX :: SExpr -> TCComp ((TcExpr, Type), Purity)
inferX e = do
  e' <- inferExpr e
  t  <- geType e'
  p  <- gePurity e'
  pure ((e', t), p)

inferB :: SExpr -> SExpr -> TCComp ((TcExpr, Type), (TcExpr, Type), Purity)
inferB l r = (\(x, px) (y, py) -> (x, y, px <> px)) <$> inferX l <*> inferX r

inferBin :: (Revisor l d o -> ExprExt -> op -> TcExpr -> TcExpr -> TcExpr)
         -> TConv -> op -> SExpr -> SExpr -> TCComp TcExpr
inferBin ctor tc o l r = do
  ((l', tl), (r', tr), p) <- inferB l r
  t' <- lM $ tc tl tr
  pure $ ctor NoH (EExt p $ pure t') o l' r'

inferNB, inferBI, inferSH :: NumOp -> SExpr -> SExpr -> TCComp TcExpr
inferNB = inferBin ENum numBinConv
inferBI = inferBin ENum biBinConv
inferSH = inferBin ENum unConvSh

inferNum :: NumOp -> SExpr -> SExpr -> TCComp TcExpr
inferNum o = case o of
  Add     -> inferNB o
  Sub     -> inferNB o
  Mul     -> inferNB o
  Div     -> inferNB o
  Rem     -> inferNB o
  LShift  -> inferSH o
  RShift  -> inferSH o
  RRShift -> inferSH o
  And     -> inferBI o
  Xor     -> inferBI o
  Or      -> inferBI o

inferUna :: (Revisor l d o -> ExprExt -> TcExpr -> TcExpr)
         -> UConv -> SExpr -> TCComp TcExpr
inferUna ctor conv e = do
  ((e', t), p) <- inferX e
  t'           <- lM $ conv t
  pure $ ctor NoH (EExt p $ pure t') e'

inferNCmp :: CmpOp -> SExpr -> SExpr -> TCComp TcExpr
inferNCmp = inferBin ECmp numBinConv

inferCmp :: CmpOp -> SExpr -> SExpr -> TCComp TcExpr
inferCmp o = case o of
  EQ -> inferBin ECmp eqOpConv o
  NE -> inferBin ECmp eqOpConv o
  LT -> inferNCmp o
  GT -> inferNCmp o
  LE -> inferNCmp o
  GE -> inferNCmp o

inferExpr :: SExpr -> TCComp TcExpr
inferExpr e = case e of
  EExpr    {} -> undefined
  ELit     {} -> inferLit e
  EVar     {} -> u
  ECast    {} -> u
  ECond    {} -> u
  EAssign  {} -> u
  EOAssign {} -> u
  ENum     {..} -> inferNum _eNumOp _eLeft _eRight
  ECmp     {..} -> inferCmp _eCmpOp _eLeft _eRight
  ELog     {..} -> inferBin ELog boolConv _eLogOp _eLeft _eRight
  ENot     {..} -> inferUna ENot (unBin [boT] boolConv) _eExpr
  EStep    {} -> u
  EBCompl  {..} -> inferUna EBCompl intUnConv _eExpr
  EPlus    {..} -> inferUna EPlus   unConv    _eExpr
  EMinus   {..} -> inferUna EMinus  unConv    _eExpr
  EMApp    {} -> u
  EArrNew  {} -> u
  EArrNewI {} -> u
  ESysOut  {} -> u