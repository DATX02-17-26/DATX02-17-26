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
import Test.QuickCheck
import Language.Haskell.Interpreter

import CoreS.Parse
import qualified CoreS.ASTUnitype as AST
import GenStrat
import SolutionContext
import EvaluationMonad
import RunJavac
import PropertyBasedTesting
import NormalizationStrategies hiding ((<>))
import InputMonad

import Normalizations
import ParseArguments

-- | The actual entry point of the application
application :: Maybe (String, String) -> FilePath -> FilePath -> EvalM ()
application gp ss dirOfModelSolutions = let compDir = "compilationDirectory" in
  withTemporaryDirectory compDir $ do
    -- Get the filepaths of the student and model solutions
    paths <- getFilePathContext ss dirOfModelSolutions

    -- Get the generator for `stdin` test data
    gen <- makeGen gp

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
    let normalizedASTs = (AST.convertCompilationUnit . executeNormalizer normalizations) <$> astContext

    -- Generate information for the teacher
    if studentSolutionMatches matches normalizedASTs then
      comment "Student solution matches a model solution"
    else
      do 
        issue "Student solution does not match a model solution"
        case compilationStatus of
          Succeeded -> runPBT compDir gen
          _         -> return ()

    return ()

-- | Create a generator from a module-function pair
makeGen :: Maybe (String, String) -> EvalM (Gen String)
makeGen Nothing = do
  logMessage "Using arbitrary generator"
  return arbitrary
makeGen (Just (mod, fun)) = do
  eg <- liftIO $ runInterpreter $ do
    loadModules [mod ++ ".hs"]
    setTopLevelModules [mod]
    interpret ("makeGenerator (" ++ fun ++ " :: InputMonad NewlineString ())") (as :: Gen String)
  case eg of
    Right g    -> return g
    Left error -> throw $ "Failed to load generator: " ++ show error

main :: IO ()
main = do
  -- Parse command line arguments
  args <- execParser argumentParser  

  -- Get all the relevant parts of the arguments
  let env                 = environment args
      studentSolution     = studentSolutionPath args
      dirOfModelSolutions = modelSolutionsPath  args
      gp                  = generatorPair args

  -- Run the actual application
  executeEvalM env $ application gp studentSolution dirOfModelSolutions
