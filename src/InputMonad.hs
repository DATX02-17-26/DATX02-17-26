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
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts 
  , ConstraintKinds 
#-}
module InputMonad where

import Test.QuickCheck
import Control.Monad.Writer
import Control.Monad

-- | A monad in which to construct exercise input
-- specifications
type InputMonad m a = WriterT m Gen a

-- | Construct a `Gen String` from an `InputMonad a`
makeGenerator :: (Monoid m, Wrapper m String) => InputMonad m a -> Gen String
makeGenerator input = (unwrap . snd) <$> runWriterT input

-- | Provide some input to the program under test
inp :: (Monoid m, Wrapper m String) => String -> InputMonad m ()
inp = tell . wrap

-- | Generate anything
anything :: (Arbitrary a, InputMonoid m) => InputMonad m a
anything = lift arbitrary

-- | A class of all monoid wrappers
class Wrapper m a where 
  wrap   :: a -> m
  unwrap :: m -> a

-- | All things wrap themselves
instance Wrapper a a where
  wrap   = id
  unwrap = id

-- | Something is only an `InputMonoid` if it's both a monoid
-- and it wraps `String`
type InputMonoid m = (Wrapper m String, Monoid m)

newtype NewlineString = NLString { unNLString :: String } deriving (Eq, Ord)

instance Show NewlineString where
  show = show . unNLString

instance Monoid NewlineString where
  mempty = NLString ""

  (NLString "") `mappend` x = x
  x `mappend` (NLString "") = x
  (NLString x) `mappend` (NLString y) = NLString $ x ++ "\n" ++ y

instance Wrapper NewlineString String where
  wrap = NLString 
  unwrap = unNLString

newtype SpaceString = SPString { unSPString :: String} deriving (Eq, Ord)

instance Show SpaceString where
  show = show . unSPString

instance Monoid SpaceString where
  mempty = SPString ""

  (SPString "") `mappend` x = x
  x `mappend` (SPString "") = x
  (SPString x) `mappend` (SPString y) = SPString $ x ++ " " ++ y

instance Wrapper SpaceString String where
  wrap = SPString 
  unwrap = unSPString
