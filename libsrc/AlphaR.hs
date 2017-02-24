module AlphaR where 

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import CoreS.AST

data Env = Env {
  mName :: Int,
  cName :: Int,
  vName :: Int,
  names :: [Map Ident Ident] 
  }
     deriving (Eq, Show)


name :: String
name = "AplhaR"

stages :: [Int]
stages = [0]

--renameClass :: ClassDecl-> State Env ClassDecl

--renameMethod :: MemberDecl -> State Env

newEnv :: Env
newEnv = Env {
  mName = 0,
  cName = 0,
  vName = 0,
  names = [Map.empty]
}





