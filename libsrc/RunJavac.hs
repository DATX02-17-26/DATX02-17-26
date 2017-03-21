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
module RunJavac where

import System.Process
import System.Exit
import System.Directory
import System.FilePath

import SolutionContext
import EvaluationMonad

-- | The result of compiling a program with `javac`
data CompilationStatus = FailedWith { stdout :: String, stderr :: String }
                       | Succeeded
                       deriving (Show, Eq) 

-- | `tryCompile dir path` tries to compile the
-- file `path` using the `javac` command, putting
-- the resulting class files in `dir`
tryCompile :: FilePath -> FilePath -> EvalM CompilationStatus
tryCompile dir path = do
  -- Run javac with the -d option
  let command = "javac -d " ++ dir ++ " " ++ path
  logMessage $ "Running the command: " ++ command 
  (exitCode, stdin, stderr) <- liftIO $ readCreateProcessWithExitCode (shell command) ""
  if not (null stderr) then
    logMessage $ "STDERR: " ++ stderr
  else
    return ()

  -- Check if the compilation was successfull
  case exitCode of
    ExitSuccess   -> return Succeeded

    -- ExitFailure 2 means javac failed because it couldn't find the file
    ExitFailure 2 -> throw $ "Javac failed without compiling"

    -- Treat all other exit codes as compilation failures
    ExitFailure _ -> return $ FailedWith stdin stderr

-- | `compileThrow dir path` tries to compile the file
-- `path` using the `javac` command, putting the resulting
-- class files in `dir`
compileThrow :: FilePath -> FilePath -> EvalM ()
compileThrow dir path = do
  result <- tryCompile dir path
  case result of
    Succeeded -> return ()
    _         -> throw $ "The file " ++ path ++ " failed to compile"

-- | `compileContext ctx dir` tries to compile all the files in the given context,
-- using the directory `dir` for the intermidiary files.
--
-- Will throw an exception if any of the model solutions fail to compile and returns
-- the compilation status of the student solution
compileContext :: SolutionContext FilePath -> FilePath -> EvalM CompilationStatus
compileContext ctx dir = do
  -- Create directories for the model and studnet solutions
  liftIO $ createDirectory (dir </> "model")
  liftIO $ createDirectory (dir </> "student")

  -- Try to compile all the model solutions
  sequence $ compileThrow (dir </> "model") <$> modelSolutions ctx

  -- Try to compile the student solution
  tryCompile (dir </> "student") (studentSolution ctx)
