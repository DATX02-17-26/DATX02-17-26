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
import Control.Monad
import System.Exit
import Options.Applicative
import Options.Applicative.Types
import Data.Semigroup
import Data.List

import SolutionContext
import EvaluationMonad
import RunJavac
import PropertyBasedTesting
import NormalizationStrategies hiding ((<>))

-- | The command line arguments
data CommandLineArguments = CMD { generatorPair       :: (String, String)
                                , studentSolutionPath :: FilePath
                                , modelSolutionsPath  :: FilePath
                                , environment         :: Env
                                } deriving Show

-- | A parser for command line arguments
arguments :: Parser CommandLineArguments
arguments =  CMD
         <$> argument generator (metavar "TEST_GENERATOR" <> help "Should be on the form module:generator")
         <*> argument str       (metavar "STUDENT_SOLUTION")
         <*> argument str       (metavar "MODEL_SOLUTIONS_DIR")
         <*> parseEnv

-- | A `ReadM` "parser" for "module:function" to specify what generator to use
generator :: ReadM (String, String)
generator = do
  s <- readerAsk
  case elemIndex ':' s of
    Nothing -> readerError "Could not parse generator, should be on the form FILE:FUNCTION"
    Just i  -> return $ splitAt i s

-- | Full parser for arguments
argumentParser :: ParserInfo CommandLineArguments
argumentParser = info (arguments <**> helper)
                 (  fullDesc
                 <> header "JAA, a program for Java Automated Assessment"
                 )

-- | The actual entry point of the application
application :: FilePath -> FilePath -> EvalM ()
application studentSolution dirOfModelSolutions = do
  -- Get the filepaths of the student and model solutions
  paths <- getFilePathContext studentSolution dirOfModelSolutions

  -- Try to compile the student and model solutions
  let compDir = "compilationDirectory"
  withTemporaryDirectory compDir $ do
    compilationStatus <- compileContext paths compDir
    case compilationStatus of
      Succeeded -> runPBT compDir
      _         -> issue  "Student solution does not compile!"

  -- Get the context from the arguments supplied
  context <- readRawContext paths

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
