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

{-# LANGUAGE TypeApplications #-}
module PropertyTesting where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad.Writer
import Control.Monad

-- | `stdin` input
type ProgramInput = String

-- | `stdout` output
type ProgramOutput = String

-- | A monad in which to construct exercise input
-- specifications
type InputMonad a = WriterT ProgramInput Gen a

-- | Construct a `Gen String` from an `InputMonad a`
makeGenerator :: InputMonad a -> Gen String
makeGenerator inp = snd <$> runWriterT inp

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
exercise0 :: InputMonad ()
exercise0 = do
  n <- abs <$> lift (arbitrary @Int)
  tell $ show n
  void $ replicateM n $ lift (arbitrary @Int) >>= \x -> tell $ " " ++ show x

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
exercise1_8 :: InputMonad ()
exercise1_8 = do
  n <- abs <$> lift (arbitrary @Double)
  tell $ show n
  n <- abs <$> lift (arbitrary @Double)
  tell $ "\n" ++ show n

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
exercise1_11 :: InputMonad ()
exercise1_11 = do
  y <- lift $ choose (0, 99 :: Int)
  tell $ leftPad 2 '0' (show y)

  m <- lift $ choose (1, 12)
  tell $ leftPad 2 '0' (show m)

  d <- lift $ choose (1, monthLength m)
  tell $ leftPad 2 '0' (show d)

monthLength :: Int -> Int
monthLength m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

leftPad :: Int -> Char -> String -> String
leftPad i c s
  | length s >= i = s
  | otherwise     = (replicate (i - length s) c) ++ s
