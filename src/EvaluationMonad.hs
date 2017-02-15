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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EvaluationMonad (
  liftIO,
  throw,
  catch,
  logMessage,
  withTemporaryDirectory,
  EvalM,
  runEvalM,
  executeEvalM,
  Env(..),
  defaultEnv,
  parseEnv 
) where
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except as E
import Control.Monad.Writer.Lazy hiding ((<>))
import Control.Monad.Reader
import qualified Control.Exception as Exc
import System.Exit
import System.Directory
import Options.Applicative
import Data.Semigroup

-- | A monad for evaluating student solutions
newtype EvalM a = EvalM { unEvalM :: ExceptT EvalError (ReaderT Env (WriterT Log IO)) a }
  deriving (Monad, Applicative, Functor, MonadWriter Log, MonadReader Env)

-- | For now we just log strings
type LogMessage = String
type Log        = [LogMessage]

-- | Evaluation errors are also just strings
type EvalError  = String

-- | The environment of the program
data Env = Env { verbose :: Bool
               , logfile :: FilePath
               }
  deriving Show

-- | The default environment
defaultEnv :: Env
defaultEnv = Env { verbose = False
                 , logfile = "logfile.log"
                 }

-- | A parser for environments
parseEnv :: Parser Env
parseEnv =  Env
        <$> switch
              (  long    "verbose"
              <> short   'v'
              <> help    "Prints log messages during execution"
              )
        <*> strOption
              (  long    "logfile"
              <> short   'l'
              <> value   "logfile.log"
              <> metavar "LOGFILE"
              <> help    "Change the default logfile produced on program crash"
              )

-- | `printLog log` converts the log to a format suitable
-- for logfiles
printLog :: Log -> String
printLog = unlines

-- | Log a message
logMessage :: LogMessage -> EvalM ()
logMessage l = do
  tell [l]
  verb <- verbose <$> ask
  if verb then
    liftIO $ putStrLn l
  else
    return ()

-- | Throw an error
throw :: EvalError -> EvalM a
throw = EvalM . throwE

-- | Catch an error
catch :: EvalM a -> (EvalError -> EvalM a) -> EvalM a
catch action handler = EvalM $ catchE (unEvalM action) (unEvalM . handler)

-- | Lift an IO action and throw an exception if the
-- IO action throws an exception
performIO :: IO a -> EvalM a
performIO io = EvalM $ do
  result <- lift $ lift $ lift $ Exc.catch (Right <$> io) (\e -> return $ Left $ show (e :: Exc.SomeException))
  case result of
    Left err -> throwE err
    Right a  -> return a

-- | A `MonadIO` instance where
-- lifting means catching and rethrowing exceptions
instance MonadIO EvalM where
  liftIO = performIO

-- | Run an `EvalM` computation
runEvalM :: Env -> EvalM a -> IO (Either EvalError a, Log)
runEvalM env = runWriterT . flip runReaderT env . runExceptT . unEvalM

-- | Execute an `EvalM logfile` computation, reporting
-- errors to the user and dumping the log to file
-- before exiting
executeEvalM :: Env -> EvalM a -> IO a
executeEvalM env eval = do
  (result, w) <- runEvalM env eval
  case result of
    Left e -> do
      putStrLn $ "Error: " ++ e
      putStrLn $ "The log has been written to " ++ (logfile env)
      writeFile (logfile env) $ printLog w
      exitFailure
    Right a -> return a

-- | Run an `EvalM` computation with a temporary directory
withTemporaryDirectory :: EvalM a -> FilePath -> EvalM a
withTemporaryDirectory evalm dir = do
  liftIO $ createDirectoryIfMissing True dir
  result <- catch evalm $ \e -> liftIO (removeDirectoryRecursive dir) >> throw e 
  liftIO $ removeDirectoryRecursive dir
  return result
