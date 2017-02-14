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

import SolutionContext
import EvaluationMonad

data CompilationStatus = FailedWith { stdout :: String, stderr :: String }
                       | Succeeded
                       deriving (Show, Eq) 

-- | `tryCompile dir path` tries to compile the
-- file `path` using the `javac` command, putting
-- the resulting class files in `dir`
tryCompile :: FilePath -> FilePath -> EvalM CompilationStatus
tryCompile dir path = do
  -- Run javac with the -d option
  let command = "javac -d" ++ dir ++ " " ++ path 
  logMessage $ "Running the command: " ++ command 
  (exitCode, stdin, stderr) <- liftIO $ readCreateProcessWithExitCode (shell command) ""

  -- Check if the compilation was successfull
  case exitCode of
    ExitSuccess   -> return Succeeded

    -- ExitFailure 2 means javac failed because it couldn't find the file
    ExitFailure 2 -> throw $ "Javac failed to find the file: " ++ path

    -- Treat all other exit codes as compilation failures
    ExitFailure _ -> return $ FailedWith stdin stderr

-- | `compileContext ctx dir` tries to compile all the files in the given context,
-- using the directory `dir` for the intermidiary files
compileContext :: SolutionContext FilePath -> FilePath
compileContext ctx dir = undefined -- TODO
