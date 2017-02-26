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

module Strategies where

import Ideas.Common.Library
import Ideas.Common.Strategy as S
import Control.Monad.State
import Data.Generics.Uniplate.DataOnly (transformBi)

import CoreS.ASTUnitype
import CoreS.ASTUnitypeUtils

-- | append an AST to the end of a block
appendStratBlock :: AST -> State Int (Strategy AST)
appendStratBlock x = do
  (b, i) <- appendBlockHole
  s <- makeStrategyAt i x
  return $ b .*. s

appendBlockHole :: State Int (Strategy AST, Int)
appendBlockHole = do
  i <- get
  put (i+1)
  return (toStrategy $ makeRule "appendBlockHole" (append i), i)
  where
    append i (Block xs) = Just (Block (xs ++ [Hole i]))
    append _ _          = Nothing

-- | add a list of ASTs to a block,
-- order is not important if there is a dependency
sequenceDependencyBlock :: [AST] -> State Int (Strategy AST)
sequenceDependencyBlock []       = return $ succeed
sequenceDependencyBlock [x]      = appendStratBlock x
sequenceDependencyBlock (x:y:ys)
  | y `dependsOn` x = (.*.) <$> (appendStratBlock x) <*> (sequenceDependencyBlock (y:ys))
  | otherwise       = (.|.) <$> ((.*.) <$> appendStratBlock x <*> sequenceDependencyBlock (y:ys))
                            <*> ((.*.) <$> appendStratBlock y <*> sequenceDependencyBlock (x:ys))

makeStrategyAt :: Int -> AST -> State Int (Strategy AST)
makeStrategyAt i ast = do
  s <- makeStrategy ast
  return $ check (\case {Hole x -> x == i; _ -> False}) .*. s

makeStrategy :: AST -> State Int (Strategy AST)
makeStrategy (Block xs) = sequenceDependencyBlock xs
makeStrategy ast = return $ toStrategy $ makeRule "makeStrategy" (\_ -> Just ast) -- This is only a test
