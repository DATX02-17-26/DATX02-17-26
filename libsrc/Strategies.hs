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

module Strategies where

import Ideas.Common.Library

import CoreS.AST

compilationUnitAppendDecl :: TypeDecl -> Rule CompilationUnit
compilationUnitAppendDecl td = makeRule "compilationUnit.append" (f td)
  where
    f :: TypeDecl -> CompilationUnit -> Maybe CompilationUnit
    f td (CompilationUnit tds) = Just $ CompilationUnit $ tds ++ [td]

blockAppendStmt :: Stmt -> Rule Block
blockAppendStmt s = makeRule "block.append" (f s)
  where
    f :: Stmt -> Block -> Maybe Block
    f stmt (Block stmts) = Just $ Block $ stmts ++ [stmt]

makeCompilationUnit :: CompilationUnit -> Strategy CompilationUnit
makeCompilationUnit (CompilationUnit tds) =
  sequenceS [ | td <- tds]
