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

-- | Normalizers for eliminating redundant statements and blocks.
module Norm.ElimRedundant (
  -- * Normalizers
    normEmptyBlock
  , normFilterEmpty
  , normFlattenBlock
  , normSingleton
  ) where

import Control.Lens ((%%~))
import Control.Lens.Extras (is)

import Util.Monad (traverseJ)
import Norm.NormCS

-- TODO allocate stages. At the moment chosen arbitrarily.
stage :: Int
stage = 8

-- | Rule to flatten a block containing statements in to parent block
-- > { s {s s} s } => {s s s s}
normFlattenBlock :: NormCUR
normFlattenBlock = makeRule' "elim_redundant.stmt.flatten_block" [stage]
                            execFlattenBlock

-- | Rule for removing empty statement blocks.
-- > {} => ;
normEmptyBlock :: NormCUR
normEmptyBlock = makeRule' "elim_redundant.stmt.empty_sblock" [stage + 1]
                           execEmptyBlock

-- | Rule for removing all empty statements from all blocks.
-- > {s ; s} => {s s}
normFilterEmpty :: NormCUR
normFilterEmpty = makeRule' "elim_redundant.stmt.filter_empty" [stage + 2]
                            execFilterEmpty

-- | Rule for a block of a single statement into the statement.
-- > { s } => s
normSingleton :: NormCUR
normSingleton = makeRule' "elim_redundant.stmt.singleton" [stage + 3]
                          execSingleton

-- | Flatten blocks
execFlattenBlock :: NormCUA
execFlattenBlock = normEvery $ \case
  Block stmts -> fmap Block $ flip traverseJ stmts $ \case
    SBlock (Block ss) -> change ss
    s                 -> unique [s]
  x           -> unique x

-- | Normalize an empty block into an empty statement.
execEmptyBlock :: NormCUA
execEmptyBlock = normEvery $ \case
  SBlock (Block []) -> change SEmpty
  x                 -> unique x

-- | Remove empty statements from blocks.
execFilterEmpty :: NormCUA
execFilterEmpty = normEvery $ \case
  Block ss -> Block <$> filterM (\case SEmpty -> change False
                                       _      -> unique True)
                                ss
  x        -> unique x

-- | Normalize block of one statement into the statement.
--
-- Exception: In control statments, local variables must be declared in a block.
-- Failure to do so results in a parse error.
execSingleton :: NormCUA
execSingleton = normEvery $ \case
  Block ss -> Block <$> mapM execSingleton' ss
  b        -> unique b

execSingleton' :: Monad m => NormArrT m Stmt
execSingleton' s = case s of
  SForB   {} -> singSi s
  SForE   {} -> singSi s
  SDo     {} -> singSi s
  SWhile  {} -> singSi s
  SIf     {} -> singSi s
  SIfElse {} -> singSi >=> sSe %%~ singStmt True $ s
  _          -> singStmt False s

singSi :: Monad m => NormArrT m Stmt
singSi = sSi %%~ singStmt True

singStmt :: Monad m => Bool -> NormArrT m Stmt
singStmt p = \case
  b@(SBlock (Block [s])) -> if p && is _SVars s then unique b else change s
  s                      -> unique s
