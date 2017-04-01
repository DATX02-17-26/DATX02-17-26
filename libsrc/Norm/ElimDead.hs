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

-- | Normalizers for eliminating dead control statements.
module Norm.ElimDead (
  -- * Normalizers
    normDeadIf
  , normDeadWhile
  , normDeadDo
  , normDeadFor
  ) where

import Norm.NormCS
import Norm.NormFor (mfiToStmt)

-- TODO allocate stages. At the moment chosen arbitrarily.
stage :: Int
stage = 200

--------------------------------------------------------------------------------
-- Exported Rules:
--------------------------------------------------------------------------------

-- | Eliminates an if statement which will never be taken.
-- > if ( false ) s => ;
normDeadIf :: NormCUR
normDeadIf = makeRule' "elim_dead.stmt.if" [stage] execDeadIf

-- | Eliminates a while statement which will never be taken.
-- > while ( false ) s => ;
normDeadWhile :: NormCUR
normDeadWhile = makeRule' "elim_dead.stmt.while" [stage] execDeadWhile

-- | Eliminates a do while statement which will only be taken once.
-- > do s while ( false ) => s 
normDeadDo :: NormCUR
normDeadDo = makeRule' "elim_dead.stmt.do" [stage] execDeadDo

-- | Eliminates a for statement which will never be taken.
-- > for ( init ; false ; update ) => init
normDeadFor :: NormCUR
normDeadFor = makeRule' "elim_dead.stmt.for" [stage] execDeadFor

--------------------------------------------------------------------------------
-- elim_dead.stmt.if:
--------------------------------------------------------------------------------

execDeadIf :: NormCUA
execDeadIf = normEvery $ \case
  SIf e _ | litFalse e -> change SEmpty
  x -> unique x

--------------------------------------------------------------------------------
-- elim_dead.stmt.while:
--------------------------------------------------------------------------------

execDeadWhile :: NormCUA
execDeadWhile = normEvery $ \case
  SWhile e _ | litFalse e -> change SEmpty
  x -> unique x

--------------------------------------------------------------------------------
-- elim_dead.stmt.do:
--------------------------------------------------------------------------------

execDeadDo :: NormCUA
execDeadDo = normEvery $ \case
  SDo e s | litFalse e -> change s
  x -> unique x

--------------------------------------------------------------------------------
-- elim_dead.stmt.for:
--------------------------------------------------------------------------------

execDeadFor :: NormCUA
execDeadFor = normEvery $ \case
  SForB mfi (Just e) _ _ | litFalse e -> change $ mfiToStmt mfi
  x -> unique x