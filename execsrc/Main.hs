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

module Main where

import System.Environment
import System.Exit
import Control.Monad
import Options.Applicative
import Data.Semigroup

import CoreS.Parse
import SolutionContext
import EvaluationMonad
import RunJavac
import NormalizationStrategies hiding ((<>))

import Normalizations

-- | The command line arguments
data CommandLineArguments = CMD { studentSolutionPath :: FilePath
                                , modelSolutionsPath  :: FilePath
                                , environment         :: Env
                                } deriving Show

-- | A parser for command line arguments
arguments :: Parser CommandLineArguments
arguments =  CMD
         <$> argument str (metavar "STUDENT_SOLUTION")
         <*> argument str (metavar "MODEL_SOLUTIONS_DIR")
         <*> parseEnv

-- | Full parser for arguments
argumentParser :: ParserInfo CommandLineArguments
argumentParser = info (arguments <**> helper)
                 (  fullDesc
                 <> header "JAA, a program for Java Automated Assessment"
                 )

-- | The actual entry point of the application
application :: FilePath -> FilePath -> EvalM ()
application ss dirOfModelSolutions = do
  -- Get the filepaths of the student and model solutions
  paths <- getFilePathContext ss dirOfModelSolutions

  -- Try to compile the student and model solutions
  compilationStatus <- compileContext paths "compilationDirectory"  
  case compilationStatus of
    Succeeded -> return ()
    _         -> issue "Student solution does not compile!"

  -- Get the contents from the arguments supplied
  convASTs <- (fmap (fmap parseConvUnit)) . (zipContexts paths) <$> readRawContents paths

  -- Convert `(FilePath, Either String AST)` in to an `EvalM AST` by throwing the parse error
  -- and alerting the user of what file threw the parse error on failure
  let convert (f, e) = either (\parseError -> throw $ "Parse error in " ++ f ++ ": " ++ parseError) return e

  -- Get the student and model solutions
  astContext <- Ctx <$>
                convert (studentSolution convASTs) <*>
                sequence [convert m | m <- modelSolutions convASTs]

  -- The normalized ASTs
  let normalizedASTs = executeNormalizer normalizations <$> astContext
  
  -- Generate information for the teacher
  if studentSolutionMatches (==) normalizedASTs then
    comment "Student solution matches a model solution"
  else
    comment "Student solution does not match a model solution"

  return ()

main :: IO ()
main = do
  -- Parse command line arguments
  args <- execParser argumentParser  

  -- Get all the relevant parts of the arguments
  let env                 = environment args
      studentSolution     = studentSolutionPath args
      dirOfModelSolutions = modelSolutionsPath  args

  -- Run the actual application
  executeEvalM env $ application studentSolution dirOfModelSolutions
