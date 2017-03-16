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
import qualified Test.QuickCheck as QC
import System.Random (split, Random, randomR)
import Test.QuickCheck.Random (QCGen)
import Data.RoseTree
import Control.Monad
import Control.Monad.Reader

data Env = Env { width :: Int, depth :: Int } deriving (Ord, Eq, Show)

defaultEnv :: Env
defaultEnv = Env 10 10

type Internal a = ReaderT Env QC.Gen (RoseTree a)

--Rose Generator wrapping the Generator from QC
newtype RoseGen a = RoseGen { unGen :: Internal a }

generate :: RoseGen a -> IO (RoseTree a)
generate (RoseGen gen) = QC.generate (runReaderT gen defaultEnv)

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
bindGenTree :: Internal a -> (a -> Internal b) -> Internal b
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

-- | Generate an arbitrary value, and all ways to shrink that value
anything :: (QC.Arbitrary a) => RoseGen a
anything = RoseGen $ do
  a <- lift $ QC.arbitrary
  return $ makeTree a
  where
    makeTree a = RoseTree a [makeTree a' | a' <- QC.shrink a]

-- TODO: Use the environment to
-- decide how much to shrink
choose :: Random a => (a,a) -> RoseGen a
choose (lo, hi) = RoseGen $ do
  a <- lift $ QC.choose (lo, hi)
  let shrinks = [] -- Use the environment to know how aggressively to shrink
  return $ RoseTree a shrinks

-- TODO: Test this
listOf :: RoseGen a -> RoseGen [a]
listOf gen = do
  n <- abs <$> anything
  replicateM n gen

-- | Generate a value such that a predicate holds for that value
suchThat :: RoseGen a -> (a -> Bool) -> RoseGen a
suchThat gen p = RoseGen $ do
  env  <- ask
  tree <- lift $ (runReaderT (unGen gen) env) `QC.suchThat` (\(RoseTree a _) -> p a)
  return $ filterTree tree p

-- | Randomly choose a generator based on some frequency
frequency ::  [(Int, RoseGen a)] -> RoseGen a
frequency gp = oneOf $ concat [replicate i g | (i, g) <- gp]

-- TODO: Is this the way we want
-- this generator to work, or do we want
-- it to shrink elements based on the list
-- we gave it as input?
elements :: [a] -> RoseGen a
elements = oneOf . map return

-- Randomly choose a generator
oneOf :: [RoseGen a] -> RoseGen a
oneOf gens = RoseGen $ do
  env <- ask
  let generators = [runReaderT (unGen g) env | g <- gens]
  lift $ QC.oneof generators
