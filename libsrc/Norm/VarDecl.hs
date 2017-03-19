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

-- | Normalizers for variable declarations.
module Norm.VarDecl where

import qualified Data.Ord as O

import Data.Maybe (isNothing)
import Data.List (sortBy)
import Control.Arrow ((&&&))
import Control.Lens (Traversal', (^?), (%%~))

import Control.Monad.Writer (WriterT, runWriterT)

import Util.Monad
import CoreS.AST
import NormalizationStrategies

stage :: Int
stage = 100 -- ALLOCATE TODO

type NormCUR = NormalizationRule CompilationUnit
type NormCUA = NormArr CompilationUnit

--------------------------------------------------------------------------------
-- Exported Rules:
--------------------------------------------------------------------------------

-- | Split variable declarations { T x [= e], y [= e], .. ; }
-- into: { T x [= e]; T y [= e] ; .. }
normSingleTVDs :: NormCUR
normSingleTVDs = makeRule' "vardecl.stmt.to_single" [stage] execSingleTVDs

-- | Split variable declaration and initialization into 2 statements.
normSplitInit :: NormCUR
normSplitInit = makeRule' "vardecl.stmt.init_split" [stage + 1] execSplitInit

-- | Sort variable declarations by type.
-- Will not sort any declarator that has initializer.
-- The sorting MUST be stable.
normSortT :: NormCUR
normSortT = makeRule' "vardecl.stmt.decl_sort_by_type" [stage + 2] execSortT

-- | Moves all variable declarations (without initializers) to the top.
-- The AST must be alpha renamed right before this is executed to preserve
-- type correctness.
normVDTop :: NormCUR
normVDTop = makeRule' "vardecl.stmt.move_to_top" [stage + 3] execVDTop

--------------------------------------------------------------------------------
-- vardecl.stmt.to_single:
--------------------------------------------------------------------------------

execSingleTVDs :: NormCUA
execSingleTVDs = normEvery $ traverseJ $ \case
  SVars (TypedVVDecl t vds) | length vds > 1 ->
    change $ SVars . TypedVVDecl t . pure <$> vds
  x -> unique [x]

--------------------------------------------------------------------------------
-- vardecl.stmt.init_split:
--------------------------------------------------------------------------------

execSplitInit :: NormCUA
execSplitInit = normEvery $ traverseJ $ \case
    s@(SVars (TypedVVDecl t [vd])) -> splitVD' t vd [s]
    x -> unique [x]

-- | VarInit to an Expr.
viToExpr :: VMType -> VarInit -> NormE Expr
viToExpr vmt vi = do
  (baseT, dims) <- mayDecline $ (typeBase &&& typeDimens) <$> vmt ^? vmType
  case vi of
    InitExpr e    -> change e
    InitArr  a    -> change $ EArrNewI baseT dims a
    HoleVarInit h -> decline

-- | VarDeclId to an LValue.
vdiToLV :: VarDeclId -> NormE LValue
vdiToLV = mayDecline . fmap LVName . (^? vdiIdent)

splitVD' :: VMType -> VarDecl -> NormArr [Stmt]
splitVD' vmt vd = withError $ \_ -> splitVD vmt vd

isFinal :: VMType -> Maybe Bool
isFinal = fmap (VMFinal ==) . (^? vmMod)

splitVD :: VMType -> VarDecl -> NormE [Stmt]
splitVD vmt vd = do
  mayDecline (isFinal vmt) >>= (`when` decline) -- can't split final.
  (vdi, mvi) <- mayDecline $ vd ^? _VarDecl
  exp        <- mayDecline mvi >>= viToExpr vmt
  lv         <- vdiToLV vdi >>= change
  pure [ SVars $ TypedVVDecl vmt [VarDecl vdi Nothing]
       , SExpr $ EAssign lv exp
       ]

--------------------------------------------------------------------------------
-- vardecl.stmt.decl_sort_by_type:
--------------------------------------------------------------------------------

execSortT :: NormCUA
execSortT = normEvery $ convEqN $ sortBy typeSort

typeSort :: Stmt -> Stmt -> Ordering
typeSort s1 s2 = toOrd $ typeSort' s1 s2

toOrd :: Maybe Bool -> Ordering
toOrd = maybe O.EQ $ \lt -> if lt then O.LT else O.GT

typeSort' :: Stmt -> Stmt -> Maybe Bool
typeSort' s1 s2 = (<) <$> varType s1 <*> varType s1

ensureMovable :: (VMType, [VarDecl]) -> Maybe ()
ensureMovable (vmt, vds) = do
  final      <- isFinal vmt
  hasNoInits <- all isNothing <$> forM vds (^? vdVInit)
  unless (not final && hasNoInits) Nothing

extTVD :: Stmt -> Maybe (VMType, [VarDecl])
extTVD = (^? sVDecl . _TypedVVDecl)

varType :: Stmt -> Maybe Type
varType s = extTVD s >>= \tvd ->
            ensureMovable tvd >> fst tvd ^? vmType

--------------------------------------------------------------------------------
-- vardecl.stmt.move_to_top:
--------------------------------------------------------------------------------

execVDTop :: NormCUA
execVDTop = normEvery $ mdBlock.bStmts %%~ vdMove

vdMove :: NormArr [Stmt]
vdMove ss = uncurry (++) <$> runWriterT (vdSteal ss)

vdSteal :: NormArrW [Stmt] [Stmt]
vdSteal = normEvery $ filterM $ \s ->
  maybe (pure True) (const $ tell [s] >> lift (change False)) $
        extTVD s >>= ensureMovable