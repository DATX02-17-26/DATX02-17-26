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
import System.Timeout
import Control.Monad.Reader
import Data.Maybe
import Data.List

import EvaluationMonad
import Data.RoseTree
import Util.RoseGen
import InputMonad

-- | Get the output from the class file `file`
solutionOutput :: Input -> FilePath -> EvalM String
solutionOutput (Input commandLineArgs stdin) file = do
  let command = "java " ++ dropExtension file ++ " " ++ intercalate " " commandLineArgs
  logMessage $ "Running the command: " ++ command

  -- Timeout 1 second
  m <- liftIO $ timeout 1000000 $ readCreateProcess (shell command) stdin
  case m of
    Nothing -> throw "Command timed out"
    Just x  -> return x

-- | Get the output of the student solution
studentOutput :: FilePath -> Input -> EvalM (Maybe String)
studentOutput dir input = do
  -- This is really inefficient and should be floated to the top level
  ss <- liftIO $ listDirectory $ dir </> "student"
  studentSolutionName <- case ss of
                          []    -> throw "Student solution missing"
                          (s:_) -> return s
  catch (Just <$> (inTemporaryDirectory (dir </> "student") $ solutionOutput input studentSolutionName))
        (\_ -> issue "Student test timeout" >> return Nothing)

-- | Get the output of every model solution
modelSolutionsOutputs :: FilePath -> Input -> EvalM [String]
modelSolutionsOutputs dir input = do
  modelSolutions <- liftIO $ listDirectory (dir </> "model")
  inTemporaryDirectory (dir </> "model") $ sequence $ solutionOutput input <$> modelSolutions

-- | Test the student solution in `dir </> "student/"` against
-- the solutions in `dir </> "model/"`
testSolutions :: FilePath -> Input -> EvalM (Maybe (String, String))
testSolutions dir input = do
  modelOutputs <- modelSolutionsOutputs dir input
  studO        <- studentOutput dir input
  return $ maybe Nothing (\s -> compareOutputs s modelOutputs) studO

compareOutputs :: String -> [String] -> Maybe (String, String)
compareOutputs student (model:[])
    | student /= model = Just (student,model)
    | otherwise = Nothing
compareOutputs student (model:ms)
    | student /= model = compareOutputs student ms
    | otherwise = Nothing

-- | Perform the relevant tests on all class files in the directory
runPBT :: FilePath -> RoseGen Input -> EvalM Bool
runPBT dir generator = do
  numTests <- numberOfTests <$> ask
  logMessage $ "Testing student solution " ++ show numTests ++ " times"
  runNumberOfTests numTests dir generator

-- | Shrink the failing input
shrink :: FilePath -> (Input, String, String) -> [RoseTree Input] -> EvalM ()
shrink dir (input, stud, mod) [] =
  issue $
       "Failed on input: " ++ show input ++ "\n"
    ++ "With\n"
    ++ "Student solution output: "
    ++ stud ++ "\n"
    ++ "Model solution output: "
    ++ mod ++ "\n"
shrink dir failing ((RoseTree input []):trees) = do
  mFailing <- testSolutions dir input
  case mFailing of
    Nothing          -> shrink dir failing trees
    Just (stud, mod) -> do
      issue $
           "Failed on input: " ++ show input ++ "\n"
        ++ "With\n"
        ++ "Student solution output: "
        ++ stud ++ "\n"
        ++ "Model solution output: "
        ++ mod ++ "\n"
shrink dir failing (tree:trees) = do
  res <- testSolutions dir (root tree)
  case res of
    Just (stud, mod) -> shrink dir (root tree, stud, mod) $ branches tree ++ trees
    Nothing          -> shrink dir failing trees

--Runs the specified number of tests
runNumberOfTests :: Int -> FilePath -> RoseGen Input -> EvalM Bool
runNumberOfTests 0 _ _ = comment "Student solution passed all tests" >> return True
runNumberOfTests numTests dir generator = do
  input   <- liftIO $ generate generator
  failing <- testSolutions dir (root input)
  case failing of
    Just (stud, mod) -> shrink dir (root input, stud, mod) (branches input) >> return False
    Nothing          -> runNumberOfTests (numTests - 1) dir generator
