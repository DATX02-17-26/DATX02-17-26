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

module GenStrat where

import Ideas.Common.Library
import Ideas.Common.Strategy as S
import Control.Monad.State
import Control.Monad as M
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
         | holeId e == Just i = ast 
         | otherwise          = e

      ruleId = "refine" ++ show i

-- | A naïve method for making a dependency-aware ordering,
-- does not create all strategies we want, we need to go to
-- a DAG approach for that
makeDependencyStrategy :: [(AST, Int)] -> State Int (Strategy AST)
makeDependencyStrategy [] = return $ succeed 
makeDependencyStrategy [(x, loc)] = genStrat loc x
makeDependencyStrategy ((x, xl):(y, yl):zs)
  | y `dependsOn` x = (.*.) <$> genStrat xl x <*> makeDependencyStrategy ((y, yl):zs)
  | otherwise       = (.|.) <$> ((.*.) <$> genStrat xl x <*> makeDependencyStrategy ((y, yl):zs))
                            <*> ((.*.) <$> genStrat xl y <*> makeDependencyStrategy ((x, yl):zs))

-- | Can we make this more DRY?
--
-- (generics?)
genStrat :: Generator
genStrat loc (Block xs)                   = do
  ids <- M.sequence [nextId | _ <- xs]
  strategy <- makeDependencyStrategy (zip xs ids)
  return $ refine (Block (map Hole ids)) loc .*. strategy
genStrat loc (MethodDecl t i params body) = (MethodDecl t i params $$ body) loc
genStrat loc (ClassDecl i body)           = (ClassDecl i $$ body) loc 
genStrat loc (ClassBody body)             = (ClassBody $$ body) loc
genStrat loc (ClassTypeDecl body)         = (ClassTypeDecl $$ body) loc
genStrat loc (CompilationUnit body)       = (CompilationUnit $$ body) loc
genStrat loc (MemberDecl body)            = (MemberDecl $$ body) loc
-- Catch all clause for things we have yet to implement
genStrat loc x = return $ refine x loc

locGen :: AST -> State Int (Int, Strategy AST)
locGen ast = do
  loc <- nextId
  strat <- genStrat loc ast
  return $ (loc, strat)

($$) :: (AST -> AST) -> AST -> Int -> State Int (Strategy AST)
($$) cons body loc = do
  (bodyLoc, bodyStrat) <- locGen body
  return $ refine (cons (Hole bodyLoc)) loc .*. bodyStrat

makeStrategy :: AST -> Strategy AST
makeStrategy ast = fst $ runState (genStrat 0 ast) 1

makeASTs :: Strategy AST -> [AST]
makeASTs strat = map lastTerm $ derivationList (\_ _ -> EQ) strat (Hole 0) 

-- | `matches a b` checks if `a` matches the strategy generated
-- by `b`
matches :: AST -> AST -> Bool
matches a b = a `elem` makeASTs (makeStrategy b)
