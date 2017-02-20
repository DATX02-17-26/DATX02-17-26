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
  , FlexibleContexts
#-}
module GeneratorExamples where

import Data.Char
import Control.Monad
import Control.Monad.Trans
import Test.QuickCheck
import Test.QuickCheck.Gen

import InputMonad 

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

{- Lab 1.11 in the course TDA540
 - Input:
 - * Read a date on the format "yymmdd"
 -
 - Task:
 - * Convert the date to the format "mm/dd/yy"
 -
 - Output:
 - * Print the date on the new format
 -}
exercise1_11 :: InputMonoid m => InputMonad m ()
exercise1_11 = do
  y <- lift $ choose (0, 99 :: Int)
  inp $ leftPad 2 '0' (show y)

  m <- lift $ choose (1, 12)
  inp $ leftPad 2 '0' (show m)

  d <- lift $ choose (1, monthLength m)
  inp $ leftPad 2 '0' (show d)

-- | Calculate the length of a month in days
monthLength :: Int -> Int
monthLength m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

-- | Break the entire `nodejs` ecosystem
leftPad :: Int -> Char -> String -> String
leftPad i c s
  | length s >= i = s
  | otherwise     = (replicate (i - length s) c) ++ s

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
