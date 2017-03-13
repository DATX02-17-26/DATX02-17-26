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

-- See https://github.com/nick8325/quickcheck/pull/136/
-- for more info and where the inspiration comes from
-- https://github.com/ambiata/disorder.hs/blob/master/disorder-jack
-- https://deque.blog/2017/02/03/code-your-own-quickcheck/
module Util.RoseGen where

import qualified Test.QuickCheck.Gen as QC
import System.Random (split, Random, randomR)
import Test.QuickCheck.Random (QCGen)
import Data.RoseTree
import Control.Monad

--Rose Generator wrapping the Generator from QC
newtype RoseGen a = RoseGen { unGen :: QC.Gen (RoseTree a) }

--Instances for Functor, Applicative and Monad for RoseGen
instance Functor RoseGen where
   fmap f (RoseGen g) = RoseGen (fmap (fmap f) g)

instance Applicative RoseGen where
  pure = RoseGen . pure . pure
  RoseGen f <*> RoseGen x = RoseGen $ (<*>) <$> f <*> x

instance Monad RoseGen where
  return = pure

  (>>=) m0 k0 =
    RoseGen $ bindGenTree (unGen m0) (unGen . k0)

-- | Used to implement '(>>=)'
bindGenTree :: QC.Gen (RoseTree a) -> (a -> QC.Gen (RoseTree b)) -> QC.Gen (RoseTree b)
bindGenTree m k =
  QC.MkGen $ \seed0 size ->
    let
      (seed1, seed2) = split seed0

      runGen :: QCGen -> QC.Gen x -> x
      runGen seed gen =
        QC.unGen gen seed size
    in
      runGen seed1 m >>= runGen seed2 . k

choose :: Random a => (a,a) -> RoseGen a
choose = undefined --RoseGen . QC.choose

listOf :: RoseGen a -> RoseGen [a]
listOf gen = undefined {-QC.sized $ \n -> do
  k <- QC.choose (0,n)
  fmap (replicateM k ) (unGen gen)
-}

frequency ::  [(Int, RoseGen a)] -> RoseGen a
frequency = RoseGen . QC.frequency . map unGenSnd
  where
    unGenSnd (i, gen) = (i, (unGen gen))

elements :: [RoseTree a] -> RoseGen a
elements = RoseGen . QC.elements

oneOf :: [RoseGen a] -> RoseGen a
oneOf = RoseGen . QC.oneof . map unGen
