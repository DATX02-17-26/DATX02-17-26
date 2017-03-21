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
module GeneratorExamples where

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
exercise0 :: InputMonoid m => InputMonad m ()
exercise0 = do
  n <- abs <$> anything @Int
  inp  $ show n
  void $ replicateM n $ anything @Int >>= inp . show

{- Lab 1.8 in the course TDA540
 - Input:
 - * Read the exchange rate between EUR and SEK
 - * Read a number of SEK
 -
 - Task:
 - * Calculate the number of EUR you
 -   get from that number of SEK
 -
 - Output:
 - * Print the number of EUR with precisely two
 -   decimal digits
 -}
exercise1_8 :: InputMonoid m => InputMonad m ()
exercise1_8 = do
  n <- abs <$> anything @Double
  inp $ show n
  n <- abs <$> anything @Double
  inp $ show n

{- 2) Skapa en kommandoradsmeny med valen p, r och q (q avslutar menyn (metoden)).
      Vid valen r och p görs en inläsning och en utskift av den inlästa strängen.
      Kommandoraden skall byggas med while och switch-satser.
 
      Exempel
      > r
      Input text > qwerty
      qwerty
      > p
      Input text > asdfgh
      asdfgh
      > q
 -}
exercise5_2 :: InputMonoid m => InputMonad m ()
exercise5_2 = do
  c <- lift $ elements ["p", "r", "q"]
  inp $ c
  case c of
    "q" -> return ()
    _   -> do
      s <- anything
      inp $ filter isAlphaNum s
      exercise5_2
