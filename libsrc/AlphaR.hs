module AlphaR where 

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import CoreS.AST

--A Context
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

--create a new Context
newContext :: State Env ()
newContext = modify (\s -> s{names = Map.empty : names s})

--exit a Context
exitContext :: State Env ()
exitContext = modify (\s -> s{names = tail(names s)})

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
   ident <- return (vName st)
   return (Ident $ "Var" ++ show ident)

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

--Renames a class to a new (Unique) Ident
--DISCLAMER Does not support private/public well,
-- 2 classes can have the same name
renameClass :: ClassDecl-> State Env ClassDecl
renameClass (ClassDecl ident body) = do
  mIdent <- lookupIdent ident
  case mIdent of
    Nothing -> do 
      name <- newClassName
      addIdent ident name
      return (ClassDecl name body)
    (Just jIdent) -> return (ClassDecl jIdent body)

--Renames a method to a new (Unique) Ident
--DISCLAMER Does not support private/public well,
-- 2 methods can have the same name
renameMethodName :: MemberDecl -> State Env MemberDecl
renameMethodName (MethodDecl mType ident formalParams block) = do 
  mIdent <- lookupIdent ident
  case mIdent of
        Nothing -> do 
          name <- newMethodName
          addIdent ident name
          return (MethodDecl mType name formalParams block)
        (Just jIdent) -> 
          return (MethodDecl mType jIdent formalParams block) 

renameMethod :: MemberDecl -> State Env MemberDecl
renameMethod member@(MethodDecl mType _ formalParams block) = do
  return member

renameFormalParam :: FormalParam -> State Env FormalParam
renameFormalParam (FormalParam vmType varDeclId) = do
  case varDeclId of
    (VarDId ident) -> do
      name <- newVarName
      addIdent ident name 
      return (FormalParam vmType (VarDId name))
    (VarDArr ident i) -> do
      name <- newVarName
      addIdent ident name 
      return (FormalParam vmType (VarDArr name i))


