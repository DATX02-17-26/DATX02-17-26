module AlphaR where 

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import CoreS.AST

type Cxt = [Map Ident Ident]

--Environment
data Env = Env {
  mName :: Int,
  cName :: Int,
  vName :: Int,
  names :: Cxt
  }
     deriving (Eq, Show)

--create a new Env
newEnv :: Env
newEnv = Env {
  mName = 0,
  cName = 0,
  vName = 0,
  names = [Map.empty]
}

--create a new block
newBlock :: State Env ()
newBlock = modify (\s -> s{names = Map.empty : names s})

--exit a block
exitBlock :: State Env ()
exitBlock = modify (\s -> s{names = tail(names s)})

--create new label
newClassName :: State Env Ident
newClassName = do
   modify (\s -> s{cName = (cName s) + 1}) 
   st <- get 
   --ident <- (cName st)
   return (Ident $ show 1)

--create new method name
newMethodName :: State Env Ident
newMethodName = do
   modify (\s -> s{mName = (mName s) + 1}) 
   st <- get 
   --ident <- (cName st)
   return (Ident $ show 1)

--create new variable name
newVarName :: State Env Ident
newVarName = do
   modify (\s -> s{vName = (vName s) + 1}) 
   st <- get 
   --ident <- (vName st)
   return (Ident $ show 1)

--add a Ident to Env
addIdent :: Ident -> Ident -> State Env ()
addIdent new old = do
  st <- get
  let (n:ns) = (names st)
  modify(\s -> s{names = (Map.insert new old n):ns})

--lookup address for var
lookupIdent :: Ident -> State Env (Maybe Ident)
lookupIdent id = state ( \s -> let res = (getIdent id (names s)) 
  in (res, s))

--helper to lookupvar
getIdent :: Ident -> Cxt -> Maybe Ident
getIdent id [] = Nothing
getIdent id (n:ns) = 
       case Map.lookup id n of
          Nothing -> getIdent id ns
          ident -> ident

name :: String
name = "AplhaR"

stages :: [Int]
stages = [0]

--renameClass :: ClassDecl-> State Env ClassDecl

--renameMethod :: MemberDecl -> State Env







