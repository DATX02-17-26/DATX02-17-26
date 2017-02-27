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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module AlphaUniplate where

import Data.Generics.Uniplate.DataOnly (universeBi, transformBi)
import Data.List
import Data.Maybe

import CoreS.AST

collectDeclarationNames :: CompilationUnit -> [String]
collectDeclarationNames cu =  methodDeclNames cu
                           ++ varDeclNames cu
                           ++ classDeclNames cu

methodDeclNames :: CompilationUnit -> [String]
methodDeclNames cu = [ n | MethodDecl _ (Ident n) _ _ <- universeBi cu]

varDeclNames :: CompilationUnit -> [String]
varDeclNames cu = [ name vdec | VarDecl vdec _ <- universeBi cu ]
  where
    name (VarDId (Ident n)) = n
    name (VarDArr (Ident n) _) = n

classDeclNames :: CompilationUnit -> [String]
classDeclNames cu = [ n | ClassDecl (Ident n) _ <- universeBi cu]

alphaRename :: CompilationUnit -> CompilationUnit
alphaRename cu = transformBi rename cu
  where
    names            = collectDeclarationNames cu
    renames          = zipWith (\n i -> (n, "v" ++ show i)) names [0..]
    rename (Ident s) = Ident $ maybe s id (lookup s renames)
