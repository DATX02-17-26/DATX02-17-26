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

module Norm.DoWToWhile where

import Util.Monad (traverseJ)
import Norm.NormCS

stage :: Int
stage = 5

-- | finds all do-while and changes them into while
-- do{s}while(e); => s; while(e){s}
normDoWToWhile :: NormCUR
normDoWToWhile = makeRule' "dowtowhile.stmt.do_while_to_while" [stage]
                            execDoWToWhile

-- dowtowhile.stmt.do_while_to_while
execDoWToWhile :: NormCUA
execDoWToWhile = normEvery $ traverseJ $ \case
  SDo expr stmt -> change [stmt, SWhile expr stmt]
  x             -> unique [x]
