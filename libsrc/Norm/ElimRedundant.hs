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
  ) where

import Util.Monad (traverseJ)
import Norm.NormCS

-- TODO allocate stages. At the moment chosen arbitrarily.
stage :: Int
stage = 50

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
