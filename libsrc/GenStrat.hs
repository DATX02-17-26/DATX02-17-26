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

type Generator = Int -> AST -> State Int (Strategy AST)

nextId :: State Int Int
nextId = do
  i <- get
  modify (+1)
  return i

holeId (Hole i) = Just i
holeId _        = Nothing

refine :: AST -> Int -> Strategy AST
refine ast i = toStrategy $ makeRule ruleId f
   where
      f p = Just $ transformBi refine' p

      refine' e
         | holeId e == Just i = expr 
         | otherwise             = e

      ruleId = "refine" ++ show i

genStrat :: Generator
genStrat loc (Block [])  = return $ refine (Block []) loc
genStrat loc (Block [x]) = do
  hid <- nextId
  xstrat <- genStrat hid x
  return $ refine (Block [Hole hid]) loc .*. xstrat
genStrat loc (Block (x:y:zs)) | y `dependsOn` x = do
  hid1 <- nextId
  hid2 <- nextId
  restOfBlock <- genStrat hid1 (Block (y:zs))
  xstrat      <- genStrat hid2 x
  return $ refine (Hole hid1) loc .*. restOfBlock
