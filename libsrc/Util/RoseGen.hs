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
import Control.Monad.Reader

--Rose Generator wrapping the Generator from QC
newtype RoseGen a = RoseGen { unGen :: ReaderT Int QC.Gen (RoseTree a) }

--Instances for Functor, Applicative and Monad for RoseGen
instance Functor RoseGen where
   fmap f (RoseGen g) = RoseGen (fmap (fmap f) g)

instance Applicative RoseGen where
  pure = RoseGen . pure . pure
  RoseGen f <*> RoseGen x = RoseGen $ (<*>) <$> f <*> x

instance Monad RoseGen where
  return = pure

  m >>= f =
    RoseGen $ bindGenTree (unGen m) (unGen . f)

-- | Used to implement '(>>=)'
bindGenTree :: ReaderT Int QC.Gen (RoseTree a) -> (a -> ReaderT Int QC.Gen (RoseTree b)) -> ReaderT Int QC.Gen (RoseTree b)
bindGenTree m k = do
  env <- ask
  lift $ QC.MkGen $ \seed0 size ->
    let
      (seed1, seed2) = split seed0

      runGen :: QCGen -> QC.Gen x -> x
      runGen seed gen =
        QC.unGen gen seed size
    in
      runGen seed1 (runReaderT m env) >>= runGen seed2 . (flip runReaderT env) . k

choose :: Random a => (a,a) -> RoseGen a
choose (lo, hi) = RoseGen $ do
  a <- lift $ QC.choose (lo, hi)
  let shrinks = []
  return $ RoseTree a shrinks

-- TODO
listOf :: RoseGen a -> RoseGen [a]
listOf gen = undefined {-QC.sized $ \n -> do
  k <- QC.choose (0,n)
  fmap (replicateM k ) (unGen gen)
-}

suchThat :: RoseGen a -> (a -> Bool) -> RoseGen a
suchThat gen p = RoseGen $ do
  env  <- ask
  tree <- lift $ (runReaderT (unGen gen) env) `QC.suchThat` (\(RoseTree a _) -> p a)
  return $ filterTree tree p

-- TODO
frequency ::  [(Int, RoseGen a)] -> RoseGen a
frequency = undefined {-RoseGen . (lift QC.frequency) . map unGenSnd
  where
    unGenSnd (i, gen) = (i, (unGen gen))-}

-- TODO: Is this the way we want
-- this generator to work, or do we want
-- it to shrink elements based on the list
-- we gave it as input?
elements :: [a] -> RoseGen a
elements = oneOf . map return

oneOf :: [RoseGen a] -> RoseGen a
oneOf gens = RoseGen $ do
  env <- ask
  let generators = [runReaderT (unGen g) env | g <- gens]
  lift $ QC.oneof generators
