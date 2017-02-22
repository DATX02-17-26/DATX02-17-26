module CoreS.Parse where

import Data.Bifunctor (first)
import Control.Monad ((>=>))

import CoreS.AST
import CoreS.Convert

import qualified Language.Java.Parser as P
import qualified Language.Java.Syntax as S

parseUnit :: String -> CConv S.CompilationUnit
parseUnit = first show . P.parser P.compilationUnit

parseConvUnit :: String -> CConv CompilationUnit
parseConvUnit = parseUnit >=> convUnit