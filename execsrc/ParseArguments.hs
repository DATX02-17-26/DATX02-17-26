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

module ParseArguments where

import Options.Applicative
import Options.Applicative.Types
import Data.Semigroup hiding (option)
import Data.List
import Test.QuickCheck
import Text.Toml

import EvaluationMonad

-- | The command line arguments
data CommandLineArguments = CMD { studentSolutionPath :: FilePath
                                , modelSolutionsPath  :: FilePath
                                , generatorPair       :: Maybe (String, String)
                                , environment         :: Env
                                } deriving Show

defaultArguments :: CommandLineArguments
defaultArguments = CMD { studentSolutionPath = "student.java"
                       , modelSolutionsPath  = "modelSolutions"
                       , generatorPair       = Nothing
                       , environment         = defaultEnv
                       }

-- | A parser for command line arguments
arguments :: Parser CommandLineArguments
arguments = argumentsWithDefault defaultArguments 

argumentsWithDefault :: CommandLineArguments -> Parser CommandLineArguments
argumentsWithDefault cmd =
  CMD
     <$> argument str       (  metavar "STUDENT_SOLUTION")
     <*> option   str       (  metavar "MODEL_SOLUTIONS_DIR"
                            <> long "model-solutions"
                            <> short 'm'
                            <> value (modelSolutionsPath cmd)
                            <> help "The directory containing all model solutions"
                            )
     <*> option   generator (  metavar "TEST_GENERATOR"
                            <> long "generator"
                            <> short 'g'
                            <> value (generatorPair cmd)
                            <> help "Should be on the form module:generator")
     <*> parseEnvDefault (environment cmd)

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
