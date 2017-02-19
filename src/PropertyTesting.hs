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

-- | Calculate the length of a month in days
monthLength :: Int -> Int
monthLength m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

-- | Break the entire `nodejs` ecosystem
leftPad :: Int -> Char -> String -> String
leftPad i c s
  | length s >= i = s
  | otherwise     = (replicate (i - length s) c) ++ s

{- 2) Skapa en kommandoradsmeny med valen p, r och q (q avslutar menyn (metoden)).
 - Vid valen r och p görs en inläsning och en utskift av den inlästa strängen.
 - Kommandoraden skall byggas med while och switch-satser.
 -
 - Exempel
 - > r
 - Input text > qwerty
 - qwerty
 - > p
 - Input text > asdfgh
 - asdfgh
 - > q
 -}

{- 3) Skriv en metod som givet en sträng (en svensk mening) översätter denna till rövarspråket
 - Se http://sv.wikipedia.org/wiki/R%C3%B6varspr%C3%A5ket
 -}

{-
 - 5) Modifiera kommandoradsmenyn så att metoderna från 3) och 4)
 - anropas då man väljer "r" respektive "p"
 - Vid valen läser man in en mening på svenska som skrivs ut översatt till
 - röverspråket eller Pig latin.
 -}

{- 6) Vi startar med strängen "MI". Kan man genom att upprepade gånger,
 - i valfri ordning, applicera reglerna nedan transformera strängen
 - till "MU"?
 - T.ex. "MI", (applicera reglerna) 1,2,2,3,1,4,2,3,4,4,2,3  ... ger detta "MU"?
 -
 - Regler
 - 1. Lägg till U till en sträng som slutar på I. Exampel: MI blir MIU.
 - 2. Fördubbla strängen efter M (ändra Mx till Mxx). Exampel: MIU blir MIUIU.
 - 3. Ersätt alla III med ett U. Exampel: MUIIIU blir MUUU.
 - 4. Ta bor alla UU. Exempel: MUUU blir MU.
 -
 - Skriv en metod som "provar allt" genom att testa alla kombinationer av reglerna.
 - Spara resultaten i ett fält av strängar (max =256+1).
 - Bryt ner metoden i undermetoder, en metod för varje regel.
 - Programmet genererar alla strängar och går därefter igenom alla och söker efter MU.
 - Om MU hittas skriver programmet ut "FOUND!"
 - (ni kan skriva ut de genererade strängarna också, som nedan)
 -
 - Exempel
 - 0 : MI
 - 1 : MIU
 - 2 : MII
 - 3 : MI
 - 4 : MI
 - 5 : MIU
 - 6 : MIUIU
 - 7 : MIU
 - 8 : MIU
 - 9 : MIIU
 - 10 : MIIII
 - 11 : MII
 - 12 : MII
 - 13 : MIU
 - 14 : MII
 - 15 : MI
 - 16 : MI
 - 17 : MIU
 - 18 : MII
 - 19 : MI
 - 20 : MI
 - 21 : MIU
 - 22 : MIUIU
 - 23 : MIU
 - 24 : MIU
 - 25 : MIUIU
 - 26 : MIUIUIUIU
 - 27 : MIUIU
 - 28 : MIUIU
 - 29 : MIU
 - 30 : MIUIU
 - 31 : MIU
 - ...
 - ... inget MU än .....
 -}
