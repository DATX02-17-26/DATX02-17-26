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
    TypeApplications
  , FlexibleContexts #-}
module Generator where

import Data.Char
import Control.Monad
import Control.Monad.Trans

import InputMonad 
import Util.RoseGen (RoseGen)

-- Lot's of examples of generators

{- Typical exercise specification:
 -  Input:
 -  * Read a number `n`
 -  * Read `n` numbers
 - 
 -  Task:
 -  * Take the sum of these `n` numbers
 -
 -  Output:
 -  * Print the sum
 -}
gen :: InputMonoid m => InputMonad m ()
gen = do
  n <- abs <$> anything @Int
  inp  $ show n
  void $ replicateM n $ anything @Int >>= inp . show
