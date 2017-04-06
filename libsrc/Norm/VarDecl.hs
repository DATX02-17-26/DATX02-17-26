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
module Norm.VarDecl (
  -- * Normalizers
    normMoveForTVD
  , normSingleTVDs
  , normVDIArrLeft
  , normSplitInit
  , normVDTop
  , normSortT
  -- * The executor functions:
  , execMoveForTVD
  , execSingleTVDs
  , execVDIArrLeft
  , execSplitInit
  , execVDTop
  , execSortT
  ) where

import qualified Data.Ord as O
import Data.Maybe (isNothing, fromMaybe)
import Data.List (sortBy, isPrefixOf)
import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((>>>))
import Control.Lens (Traversal', (^?), (%%~), (%~), (.~))
import Control.Monad.Writer (Writer, runWriter)

import Util.Monad (traverseJ, sortByM)
import Class.Sizeables (growN)

import Norm.NormCS

stage :: Int
stage = 9 -- ALLOCATE TODO

--------------------------------------------------------------------------------
-- Exported Rules:
--------------------------------------------------------------------------------

-- | Moves the variable declarations (+ any inits) out of a basic for loop:
-- > for ( T x, [y..] ; cond ; post ) si
-- normalizes to:
-- > T x, [y..] ; for ( ; cond ; post ) si
normMoveForTVD :: NormCUR
normMoveForTVD = makeRule' "vardecl.stmt.move_bfor_tvd" [stage] execMoveForTVD

-- | Split variable declarations
-- > { T x [= e], y [= e], .. ; }
-- into:
-- > { T x [= e]; T y [= e] ; .. }
normSingleTVDs :: NormCUR
normSingleTVDs = makeRule' "vardecl.stmt.to_single" [stage + 1] execSingleTVDs

-- | Move array dimensions in VarDeclId to the type, i.e:
-- > T x[].. ;  ==>  T[].. x ;
-- Requires that "vardecl.stmt.to_single" has been run before.
normVDIArrLeft :: NormCUR
normVDIArrLeft = makeRule' "vardecl.stmt.array_type" [stage + 2] execVDIArrLeft

-- | Split variable declaration and initialization into 2 statements.
normSplitInit :: NormCUR
normSplitInit = makeRule' "vardecl.stmt.init_split" [stage + 3] execSplitInit

-- | Moves all variable declarations (without initializers) to the top.
-- The AST must be alpha renamed right before this is executed to preserve
-- type correctness.
normVDTop :: NormCUR
normVDTop = makeRule' "vardecl.stmt.move_to_top" [stage + 4] execVDTop

-- | Sort variable declarations by type.
-- Will not sort any declarator that has initializer.
-- The sorting MUST be stable.
normSortT :: NormCUR
normSortT = makeRule' "vardecl.stmt.decl_sort_by_type" [stage + 5] execSortT

--------------------------------------------------------------------------------
-- vardecl.stmt.move_bfor_tvd:
--------------------------------------------------------------------------------

execMoveForTVD :: NormCUA
execMoveForTVD = normEvery $ traverseJ $ \x -> case x of
  SForB (Just (FIVars tvd)) me mps si ->
    change [SVars tvd, sForInit .~ Nothing $ x]
  x -> unique [x]

--------------------------------------------------------------------------------
-- vardecl.stmt.to_single:
--------------------------------------------------------------------------------

execSingleTVDs :: NormCUA
execSingleTVDs = normEvery $ traverseJ $ \case
  SVars (TypedVVDecl t vds) | length vds > 1 ->
    change $ SVars . TypedVVDecl t . pure <$> vds
  x -> unique [x]

--------------------------------------------------------------------------------
-- vardecl.stmt.array_type:
--------------------------------------------------------------------------------

execVDIArrLeft :: NormCUA
execVDIArrLeft = normEvery $ withError' $ \case
  (TypedVVDecl vmt [vd]) -> do
    (vdi,  mvi) <- mayDecline $ vd  ^? _VarDecl
    (vdi', dim) <- first VarDId <$> mayDecline (vdi ^? _VarDArr)
    let vmt' = vmType %~ growN (fromInteger dim) $ vmt
    change $ TypedVVDecl vmt' [VarDecl vdi' mvi]
  x -> unique x

--------------------------------------------------------------------------------
-- vardecl.stmt.init_split:
--------------------------------------------------------------------------------

execSplitInit :: NormCUA
execSplitInit = normEvery $ traverseJ $ \case
    s@(SVars (TypedVVDecl t [vd])) -> splitVD' t vd [s]
    x -> unique [x]

-- | VarInit to an Expr.
viToExpr :: VMType -> VarDeclId -> VarInit -> NormE Expr
viToExpr vmt vdi vi = do
  (baseT, dims) <- mayDecline $ (typeBase &&& typeDimens) <$> vmt ^? vmType
  let dims' = fromMaybe 0 $ vdi ^? vdiDimen
  case vi of
    InitExpr e    -> change e
    InitArr  a    -> change $ EArrNewI baseT (dims + dims') a
    HoleVarInit h -> decline

-- | VarDeclId to an LValue.
vdiToLV :: VarDeclId -> NormE LValue
vdiToLV = mayDecline . fmap singVar . (^? vdiIdent)

splitVD' :: VMType -> VarDecl -> NormArr [Stmt]
splitVD' vmt vd = withError $ \_ -> splitVD vmt vd

isFinal :: VMType -> Maybe Bool
isFinal = fmap (VMFinal ==) . (^? vmMod)

splitVD :: VMType -> VarDecl -> NormE [Stmt]
splitVD vmt vd = do
  mayDecline (isFinal vmt) >>= (`when` decline) -- can't split final.
  (vdi, mvi) <- mayDecline $ vd ^? _VarDecl
  exp        <- mayDecline mvi >>= viToExpr vmt vdi
  lv         <- vdiToLV vdi >>= change
  pure [ SVars $ TypedVVDecl vmt [VarDecl vdi Nothing]
       , SExpr $ EAssign lv exp
       ]

--------------------------------------------------------------------------------
-- vardecl.stmt.decl_sort_by_type:
--------------------------------------------------------------------------------

execSortT :: NormCUA
execSortT = normEvery $ sortByM $ curry $ (varType *** varType) >>> \case
  (Nothing, Nothing) -> pure   O.EQ
  (Just t1, Nothing) -> pure   O.LT
  (Nothing, Just t2) -> change O.GT
  (Just t1, Just t2) -> let o = compare t1 t2
                        in if o == O.GT then change o else pure o

varType :: Stmt -> Maybe Type
varType s = extTVD s >>= \tvd -> ensureMovable tvd >> fst tvd ^? vmType

extTVD :: Stmt -> Maybe (VMType, [VarDecl])
extTVD = (^? sVDecl . _TypedVVDecl)

ensureMovable :: (VMType, [VarDecl]) -> Maybe ()
ensureMovable (vmt, vds) = do
  final      <- isFinal vmt
  hasNoInits <- all isNothing <$> forM vds (^? vdVInit)
  when (final || not hasNoInits) Nothing

--------------------------------------------------------------------------------
-- vardecl.stmt.move_to_top:
--------------------------------------------------------------------------------

travMD :: Traversal' CompilationUnit MemberDecl
travMD = cuTDecls.traverse.tdClass.cdBody.cbDecls.traverse.declMem

execVDTop :: NormCUA
execVDTop = travMD.mdBlock.bStmts %%~ vdMove

vdMove :: NormArr [Stmt]
vdMove ss = let (ssF, ssT) = second reverse $ runWriter (normEveryT vdSteal ss)
            in (if ssT `isPrefixOf` ss then unique else change) $ ssT ++ ssF

vdSteal :: [Stmt] -> Writer [Stmt] [Stmt]
vdSteal = filterM $ \s -> maybe (pure True) (const $ tell [s] >> pure False) $
                                extTVD s >>= ensureMovable
