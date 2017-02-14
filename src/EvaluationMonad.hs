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
  logMessage,
  EvalM,
  runEvalM,
  executeEvalM
) where
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except as E
import Control.Monad.Fail
import Control.Monad.Writer.Lazy
import Control.Exception hiding (throw)
import System.Exit

-- | A monad for evaluating student solutions
newtype EvalM a = EvalM { unEvalM :: ExceptT EvalError (WriterT Log IO) a }
  deriving (Monad, Applicative, Functor, MonadWriter Log)

-- | For now we just log strings
type LogMessage = String
type Log        = [LogMessage]

-- | `printLog log` converts the log to a format suitable
-- for logfiles
printLog :: Log -> String
printLog = unlines

-- | Evaluation errors are also just strings
type EvalError  = String

-- | Log a message
logMessage :: LogMessage -> EvalM ()
logMessage l = tell [l]

-- | Throw an error
throw :: EvalError -> EvalM a
throw = EvalM . throwE

-- | Lift an IO action and throw an exception if the
-- IO action throws an exception
performIO :: IO a -> EvalM a
performIO io = EvalM $ do
  result <- lift $ lift $ catch (Right <$> io) (\e -> return $ Left $ show (e :: SomeException))
  case result of
    Left err -> throwE err
    Right a  -> return a

-- | A `MonadIO` instance where
-- lifting means catching and rethrowing exceptions
instance MonadIO EvalM where
  liftIO = performIO

-- | Run an `EvalM` computation
runEvalM :: EvalM a -> IO (Either EvalError a, Log)
runEvalM = runWriterT . runExceptT . unEvalM

-- | Execute an `EvalM logfile` computation, reporting
-- errors to the user and dumping the log to file
-- before exiting
executeEvalM :: FilePath -> EvalM a -> IO a
executeEvalM logfile eval = do
  (result, w) <- runEvalM eval
  case result of
    Left e -> do
      putStrLn $ "Error: " ++ e
      putStrLn $ "The log has been written to " ++ logfile
      writeFile logfile $ printLog w
      exitFailure
    Right a -> return a