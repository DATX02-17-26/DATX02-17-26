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

{-# LANGUAGE DataKinds, TypeOperators, TypeApplications, ConstraintKinds #-}

module Core.SVec (
  Min1, SVec, M1Vec (..), minOne, minOneE, toM1Vec, m1ToList, slen
  ) where

import Data.Sized (Sized, toList, fromList)
import Data.Type.Equality ((:~:) (..))
import Data.Singletons (toSing, fromSing, SomeSing (..))
import Data.Singletons.Prelude.Bool (Sing (..))
import Data.Singletons.Prelude.Ord ((:>), (%:>))
import Data.Singletons.TypeLits (Nat, SNat, Sing (..))

type Min1 (n :: Nat)   = (n :> 0) ~ True
type SVec (n :: Nat) a = Sized [] n a

data M1Vec a where
  M1Vec :: Min1 n => SNat n -> SVec n a -> M1Vec a

instance Eq a => Eq (M1Vec a) where
  (M1Vec _ va) == (M1Vec _ vb) = toList va == toList vb

instance Ord a => Ord (M1Vec a) where
  compare (M1Vec _ va) (M1Vec _ vb) = toList va `compare` toList vb

instance Show a => Show (M1Vec a) where
  showsPrec d (M1Vec s v) = showParen (d /= 0) $
    showString $ unwords ["M1Vec", show $ fromSing s, show $ toList v]

minOneE :: Either e ((n :> 0) :~: True) -> SNat n
        -> Either e ((n :> 0) :~: True)
minOneE e n = maybe e pure (minOne n)

minOne :: SNat n -> Maybe ((n :> 0) :~: True)
minOne n = case n %:> (SNat @0) of
  STrue  -> Just Refl
  SFalse -> Nothing

slen :: [a] -> SomeSing Nat
slen = toSing . foldr (const (+1)) 0

toM1Vec :: [a] -> Maybe (M1Vec a)
toM1Vec xs = case slen xs of
  SomeSing n -> do
    Refl <- minOne n
    M1Vec n <$> fromList n xs

m1ToList :: M1Vec a -> [a]
m1ToList (M1Vec _ a) = toList a