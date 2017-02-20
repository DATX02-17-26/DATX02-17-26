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
module PropertyBasedTesting where

import System.FilePath
import System.Process
import System.Directory

import InputMonad
import EvaluationMonad

{- TODO:
 - * Add timeout, throw exception when model solutions time out
 -   but issue feedback when a student solution times out
 -}

-- | Get the output from the class file `file`
solutionOutput :: String -> FilePath -> EvalM String
solutionOutput stdin file = liftIO $ readCreateProcess (shell $ "java " ++ file) stdin

-- | Get the output of the student solution
studentOutput :: FilePath -> String -> EvalM String
studentOutput dir input = do
  -- This is really inefficient and should be floated to the top level
  ss <- liftIO $ listDirectory $ dir </> "student"
  studentSolutionName <- case ss of
                          []    -> throw "Student solution missing"
                          (s:_) -> return s
  solutionOutput input (dir </> "student" </> studentSolutionName)

-- | Get the output of every model solution
modelSolutionsOutputs :: FilePath -> String -> EvalM [String]
modelSolutionsOutputs dir input = do
  modelSolutions <- liftIO $ listDirectory (dir </> "model")
  sequence $ solutionOutput input <$> modelSolutions

-- | Test the student solution in `dir </> "student/"` against
-- the solutions in `dir </> "model/"`
testSolutions :: FilePath -> String -> EvalM Bool
testSolutions dir input = do
  modelOutputs <- modelSolutionsOutputs dir input

  studO <- studentOutput dir input

  return $ or [studO == output | output <- modelOutputs]

-- | Perform the relevant tests on all class files in the directory
-- `dir`, returns `True` if the student solution passes all tests
runPBT :: FilePath -> EvalM Bool
runPBT dir = return False
