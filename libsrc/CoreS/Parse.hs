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

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module CoreS.Parse (
  -- * Classes
    LJParser
  -- * Operations
  , ljParser
  , parse
  , parseConv
  ) where

import Data.Bifunctor (first)
import Control.Monad ((>=>))

import CoreS.AST
import CoreS.Convert

import Text.Parsec (Parsec)
import qualified Language.Java.Lexer  as L
import qualified Language.Java.Parser as P
import qualified Language.Java.Syntax as S

type Mod a = [S.Modifier] -> a

--------------------------------------------------------------------------------
-- Parser class:
--------------------------------------------------------------------------------

-- | Determines the parser for some term of type t in S.
class LJParser t where
  -- | Get the parser for the term t.
  ljParser :: Parsec [L.L L.Token] () t

--------------------------------------------------------------------------------
-- Operations:
--------------------------------------------------------------------------------

-- | Parses a term in S.
parse :: LJParser t => String -> CConv t
parse = first show . P.parser ljParser

-- | Parses a term in S and then converts it into the corresponding term in
-- the CoreS.AST.
parseConv :: (LJParser t, ToCoreS t) => String -> CConv (Repr t)
parseConv = parse >=> toCoreS

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance LJParser S.CompilationUnit     where ljParser = P.compilationUnit
instance LJParser S.PackageDecl         where ljParser = P.packageDecl
instance LJParser S.ImportDecl          where ljParser = P.importDecl
instance LJParser (Maybe S.TypeDecl)    where ljParser = P.typeDecl
instance LJParser (Mod S.ClassDecl)     where ljParser = P.classDecl
instance LJParser (Mod S.InterfaceDecl) where ljParser = P.interfaceDecl
instance LJParser (Mod S.MemberDecl)    where ljParser = P.memberDecl
instance LJParser [S.FormalParam]       where ljParser = P.formalParams
instance LJParser S.FormalParam         where ljParser = P.formalParam
instance LJParser S.Modifier            where ljParser = P.modifier
instance LJParser [S.VarDecl]           where ljParser = P.varDecls
instance LJParser S.VarDecl             where ljParser = P.varDecl
instance LJParser S.Block               where ljParser = P.block
instance LJParser S.BlockStmt           where ljParser = P.blockStmt
instance LJParser S.Stmt                where ljParser = P.stmt
instance LJParser S.Exp                 where ljParser = P.exp
instance LJParser S.Literal             where ljParser = P.literal
instance LJParser S.Type                where ljParser = P.ttype
instance LJParser S.PrimType            where ljParser = P.primType
instance LJParser S.RefType             where ljParser = P.refType
instance LJParser S.ClassType           where ljParser = P.classType
instance LJParser (Maybe S.Type)        where ljParser = P.resultType
instance LJParser [S.TypeParam]         where ljParser = P.typeParams
instance LJParser S.TypeParam           where ljParser = P.typeParam
instance LJParser S.Name                where ljParser = P.name
instance LJParser S.Ident               where ljParser = P.ident