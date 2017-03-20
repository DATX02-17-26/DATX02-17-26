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

{-# LANGUAGE ScopedTypeVariables #-}

module Norm.VarDeclTest where

import Test.Tasty
import Test.Tasty.Golden.Advanced

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (except)

import Util.Monad (rebase)

import CoreS.Parse
import CoreS.ConvBack
import Norm.NormCS
import Norm.VarDecl

goldenFileM :: TestName -> FilePath -> FilePath
            -> (String -> String -> IO (Maybe String))
            -> TestTree
goldenFileM n g t m = goldenTest n (readFile g) (readFile t) m (const $ pure ())

goldenFileET :: Show e
            => TestName -> FilePath -> FilePath
            -> (String -> String -> ExceptT e IO a)
            -> TestTree
goldenFileET n g t m = goldenFileM n g t $ \g' t' ->
  either (pure . show) (const Nothing) <$> runExceptT (m g' t')

goldenFileE :: Show e
            => TestName -> FilePath -> FilePath
            -> (String -> String -> Either e a)
            -> TestTree
goldenFileE n g t m = goldenFileM n g t $ \g' t' ->
  pure $ either (pure . show) (const Nothing) (m g' t')

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT e = rebase $ except e


normalizers :: [NormCUR]
normalizers = [ normMoveForTVD
              , normSingleTVDs
              , normVDIArrLeft
              , normSplitInit
              , normVDTop
              , normSortT
              ]

unit_vardecl_test_1 = goldenFileE "unit_vardecl_test_1"
  "fixtures/norm/vardecl/Source1.java"
  "fixtures/norm/vardecl/Expected1.java" $
  \gCST tCST -> do
    (gAST :: CompilationUnit) <- parseConv gCST
    (tAST :: CompilationUnit) <- parseConv tCST
    let tASTn = executeNormalizer normalizers tAST
    if gAST == tASTn
    then pure "SUCCESS: ASTs matched"
    else do
      tCSTn <- prettyCore' gAST
      Left $ unlines
        [ "FAILURE: Normalized AST does not match golden."
        , "golden CST:", gCST
        , "test CST:", tCST
        , "test CST, normalized:", tCSTn
        ]

allTests :: TestTree
allTests = testGroup "Norm.VarDecl tests" [unit_vardecl_test_1]