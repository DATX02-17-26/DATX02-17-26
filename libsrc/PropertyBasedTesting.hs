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


{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , ConstraintKinds
  , TypeApplications
  #-}

module PropertyBasedTesting where

import Control.Monad.Writer
import Control.Monad

import System.FilePath
import System.Process
import System.Directory
import System.Timeout
import Control.Monad.Reader

import InputMonad

import EvaluationMonad
import Data.RoseTree
import Util.RoseGen
import IOWrapper
import CoreS.AST
import SolutionContext


-- | Get the output from the class file `file`
solutionOutput :: String -> FilePath -> EvalM String
solutionOutput stdin file = do
  let command = "java " ++ dropExtension file
  logMessage $ "Running the command: " ++ command

  -- Timeout 1 second
  m <- liftIO $ timeout 1000000 $ readCreateProcess (shell command) stdin
  case m of
    Nothing -> throw "Command timed out"
    Just x  -> return x

-- | Get the output of the student solution
studentOutput :: FilePath -> String -> EvalM (Maybe String)
studentOutput dir input = do
  -- This is really inefficient and should be floated to the top level
  ss <- liftIO $ listDirectory $ dir </> "student"
  studentSolutionName <- case ss of
                          []    -> throw "Student solution missing"
                          (s:_) -> return s
  catch (Just <$> (inTemporaryDirectory (dir </> "student") $ solutionOutput input studentSolutionName))
        (\_ -> issue "Student test timeout" >> return Nothing)

-- | Get the output of every model solution
modelSolutionsOutputs :: FilePath -> String -> EvalM [String]
modelSolutionsOutputs dir input = do
  modelSolutions <- liftIO $ listDirectory (dir </> "model")
  inTemporaryDirectory (dir </> "model") $ sequence $ solutionOutput input <$> modelSolutions

-- | Test the student solution in `dir </> "student/"` against
-- the solutions in `dir </> "model/"`
testSolutions :: FilePath -> String -> EvalM Bool
testSolutions dir input = do
  modelOutputs <- modelSolutionsOutputs dir input
  studO <- studentOutput dir input
  maybe (return False) (\s -> compareOutputs s modelOutputs) studO

--Compares the output of all model solutions to 1 student solution
compareOutputs :: String -> [String] -> EvalM Bool
compareOutputs _ [] = return True
compareOutputs student (model:ms) = do
  if student == model then
    compareOutputs student ms
  else do
    issue $ "Student output: " ++ student ++ "\n    Model output: " ++ model
    return False

-- | Perform the relevant tests on all class files in the directory
runPBT :: FilePath -> RoseGen String -> SolutionContext FilePath-> EvalM Bool
runPBT dir generator paths = do
  numTests <- numberOfTests <$> ask
  logMessage $ "Testing student solution " ++ show numTests ++ " times"
  runNumberOfTests numTests dir generator
  {- if runNumberOfTests numTests dir generator then do
    decls <- printWrappedSolutions dir paths
    gens <- map makeGen decls
    runNumberOfTests 2 dir $ head $ head gens
  else do
        decls <- liftIO $ printWrappedSolutions dir paths
        gens <- map makeGen decls
        runNumberOfTests 5 dir $ head decls $ head gens -}

shrink :: FilePath -> RoseTree String -> EvalM ()
shrink dir tree = issue $ "Failed with: " ++ (root tree)

--Runs the specified number of tests
runNumberOfTests :: Int -> FilePath -> RoseGen String -> EvalM Bool
runNumberOfTests 0 _ _ = comment "Student solution passed all tests" >> return True
runNumberOfTests numTests dir generator = do
  input  <- liftIO $ generate generator
  passed <- testSolutions dir (root input)
  if passed then
    runNumberOfTests (numTests - 1) dir generator
  else
    shrink dir input >> return False

makeGen :: InputMonoid m => Decl -> InputMonad m ()
makeGen (MemberDecl (MethodDecl _ _ formalParams _)) =
  inp $ map make $ map makeType formalParams
    where
      make t = case t of
        PrimT pt -> show $ lift $ makePT pt
        StringT -> lift $ anything @String
        _ -> error "Wrong Type"

      makePT pt =  case pt of
        BoolT -> anything @Bool
        ByteT  -> choose(-128,127) @Int
        ShortT  -> choose(-32768, 32767) @Int
        IntT  -> anything @Int
        LongT  -> anything @Float
        CharT  -> anything @Char
        FloatT  ->  anything @Float
        DoubleT  -> anything @Double
