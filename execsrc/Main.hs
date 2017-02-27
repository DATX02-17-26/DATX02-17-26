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
import Options.Applicative.Types
import Data.Semigroup hiding (option)
import Data.List
import Test.QuickCheck
import Language.Haskell.Interpreter

import CoreS.Parse
import SolutionContext
import EvaluationMonad
import RunJavac
import PropertyBasedTesting
import NormalizationStrategies hiding ((<>))
import InputMonad

import Normalizations

-- | The command line arguments
data CommandLineArguments = CMD { studentSolutionPath :: FilePath
                                , modelSolutionsPath  :: FilePath
                                , generatorPair       :: Maybe (String, String)
                                , environment         :: Env
                                } deriving Show

-- | A parser for command line arguments
arguments :: Parser CommandLineArguments
arguments =  CMD
         <$> argument str       (metavar "STUDENT_SOLUTION")
         <*> argument str       (metavar "MODEL_SOLUTIONS_DIR")
         <*> option   generator (  metavar "TEST_GENERATOR"
                                <> long "generator"
                                <> short 'g'
                                <> value Nothing
                                <> help "Should be on the form module:generator")
         <*> parseEnv

-- | A `ReadM` "parser" for "module:function" to specify what generator to use
generator :: ReadM (Maybe (String, String))
generator = do
  s <- readerAsk
  case elemIndex ':' s of
    Nothing -> readerError "Could not parse generator, should be on the form FILE:FUNCTION"
    Just i  -> return (Just (tail <$> splitAt i s))

-- | Full parser for arguments
argumentParser :: ParserInfo CommandLineArguments
argumentParser = info (arguments <**> helper)
                 (  fullDesc
                 <> header "JAA, a program for Java Automated Assessment"
                 )

-- | The actual entry point of the application
application :: Gen String -> FilePath -> FilePath -> EvalM ()
application gen ss dirOfModelSolutions = do
  -- Get the filepaths of the student and model solutions
  paths <- getFilePathContext ss dirOfModelSolutions

  -- Try to compile the student and model solutions
  let compDir = "compilationDirectory"
  withTemporaryDirectory compDir $ do
    compilationStatus <- compileContext paths compDir
    case compilationStatus of
      Succeeded -> return ()
      _         -> issue  "Student solution does not compile!"

    -- Get the contents from the arguments supplied
    convASTs <- (fmap (fmap parseConvUnit)) . (zipContexts paths) <$> readRawContents paths

    -- Convert `(FilePath, Either String AST)` in to an `EvalM AST` by throwing the parse error
    -- and alerting the user of what file threw the parse error on failure
    let convert (f, e) = either (\parseError -> throw $ "Parse error in " ++ f ++ ": " ++ parseError) return e

    -- Get the student and model solutions
    astContext <- Ctx <$>
                  (logMessage "Parsing student solution" >> convert (studentSolution convASTs)) <*>
                  sequence [logMessage ("Parsing model solution: " ++ (fst m)) >> convert m | m <- modelSolutions convASTs]

    -- The normalized ASTs
    let normalizedASTs = executeNormalizer normalizations <$> astContext
    
    -- Generate information for the teacher
    if studentSolutionMatches (==) normalizedASTs then
      comment "Student solution matches a model solution"
    else
      do 
        issue "Student solution does not match a model solution"
        case compilationStatus of
          Succeeded -> runPBT compDir gen
          _         -> return ()

    return ()

main :: IO ()
main = do
  -- Parse command line arguments
  args <- execParser argumentParser  

  -- Get all the relevant parts of the arguments
  let env                 = environment args
      studentSolution     = studentSolutionPath args
      dirOfModelSolutions = modelSolutionsPath  args
      gp                  = generatorPair args

  g <- case gp of
        Nothing         -> return arbitrary
        Just (mod, fun) -> do
          Right g <- runInterpreter $ do
            loadModules [mod ++ ".hs"]
            setTopLevelModules [mod]
            interpret ("makeGenerator (" ++ fun ++ " :: InputMonad NewlineString ())") (as :: Gen String)
          return g

  -- Run the actual application
  executeEvalM env $ application g studentSolution dirOfModelSolutions
