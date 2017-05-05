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

{-# LANGUAGE LambdaCase #-}

-- | Normalizer for transforming compound assignment in to assignment.
module Norm.SumsOfProducts (normSOP) where

import Norm.NormCS

stage :: Int
stage = 5

-- | Numerical expressions to SOP form
normSOP :: NormCUR
normSOP = makeRule' "sums_of_products.expr" [stage]
                    execSOP

-- | executes normalization of compund assignments
execSOP :: NormCUA
execSOP = normEvery $ \case
  ENum Mul (ENum Add x y) z   -> change $ add (mul x z) (mul y z)
  ENum Mul z (ENum Add x y)   -> change $ add (mul z x) (mul z y)
  ENum Add x y                -> if add x y /= (ENum Add x y)
                                 then change $ add x y
                                 else unique $ add x y
  ENum Mul x y                -> if mul x y /= (ENum Mul x y)
                                 then change $ mul x y
                                 else unique $ mul x y
  x                           -> unique x

is :: Integer -> Expr -> Bool
is i = \case
  ELit (Int j)    -> i == j
  ELit (Word j)   -> i == j
  ELit (Float j)  -> fromInteger i == j
  ELit (Double j) -> fromInteger i == j
  _               -> False

mul :: Expr -> Expr -> Expr
mul = curry $ \case
  (ENum Add a b, r) -> add (mul a r) (mul b r)
  (l, ENum Add a b) -> add (mul l a) (mul l b)
  (l, r)
    | is 1 l    ->  r
    | is 1 r    ->  l
    | otherwise ->  ENum Mul l r

add :: Expr -> Expr -> Expr
add = curry $ \case
  (x, ENum Add y z) -> add (add x y) z
  (x, y)
    | is 0 x    -> y
    | is 0 y    -> x
    | otherwise -> ENum Add x y
