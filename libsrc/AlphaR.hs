module AlphaR (alphaRenaming) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import CoreS.AST
import Data.Maybe
import NormalizationStrategies (makeRule, NormalizationRule)
import Norm.NormM

alphaRenaming :: NormalizationRule CompilationUnit
alphaRenaming = makeRule (convMayN execute) name stages

name :: String
name = "AlphaR"

stages :: [Int]
stages = [0]

--A Context
--might need a [Map Ident (Map Ident Ident)] where first ident is class
--also for objects, need to decide what type they are, in order to search
--correct class for them
type Cxt = [Map Ident Ident]
type MCxt = [Map Ident Ident]

--Environment
data Env = Env {
  mName :: Int,
  cName :: Int,
  vName :: Int,
  varNames :: Cxt,
  metNames :: MCxt
  }
     deriving (Eq, Show)

--create a new Env
newEnv :: Env
newEnv = Env {
  mName = 0,
  cName = 0,
  vName = 0,
  varNames = [Map.empty],
  metNames = [Map.empty]
}

--create a new Context
newContext :: State Env ()
newContext = modify (\s -> s{varNames = Map.empty : varNames s})

--new method context
newMethodCxt :: State Env ()
newMethodCxt = modify (\s -> s{metNames = Map.empty : metNames s})

--exit a Context
exitContext :: State Env ()
exitContext = modify (\s -> s{varNames = tail(varNames s)})

--exit a method context
exitMethodCxt :: State Env ()
exitMethodCxt = modify (\s -> s{metNames = tail(metNames s)})

--create new label
newClassName :: Ident -> State Env Ident
newClassName old = do
   modify (\s -> s{cName = (cName s) + 1})
   st <- get
   name  <- return (cName st)
   let new = (Ident $ "Class" ++ show name)
   addIdent new old



--create new variable name if an old does not exist
newVarName :: Ident -> State Env Ident
newVarName old = do
   mIdent <- lookupIdent old
   case mIdent of
     Just new -> return new
     Nothing -> do
      modify (\s -> s{vName = (vName s) + 1})
      st    <- get
      name  <- return (vName st)
      let new =  (Ident $ "var" ++ show name)
      addIdent new old

--add a var Ident to Env return the new var ident
addIdent :: Ident -> Ident -> State Env Ident
addIdent new old = do
  st <- get
  let (n:ns) = (varNames st)
  modify(\s -> s{varNames = (Map.insert old new n):ns})
  return new

findVarName :: Ident -> State Env Ident
findVarName i = do
  mIdent <- lookupIdent i
  case mIdent of
    Just new -> return new
    Nothing -> return i

--lookup address for var
lookupIdent :: Ident -> State Env (Maybe Ident)
lookupIdent i = do
  st <- get
  let cxt = (varNames st) in
    return $ getIdent i cxt

--helper to lookupIdent
getIdent :: Ident -> Cxt -> Maybe Ident
getIdent i [] = Nothing
getIdent i (n:ns) =
       case Map.lookup i n of
          Nothing -> getIdent i ns
          ident -> ident

--create new method name
newMethodName :: Ident -> State Env Ident
newMethodName old = do
   modify (\s -> s{mName = (mName s) + 1})
   st <- get
   name  <- return (mName st)
   let new = (Ident $ "method" ++ show name)
   addMethod new old

--add method ident to env and return new method ident
addMethod :: Ident -> Ident -> State Env Ident
addMethod new old = do
  st <- get
  let (m:ms) = (metNames st)
  modify(\s -> s{metNames = (Map.insert old new m):ms})
  return new

--lookup address for method
lookupMethod :: Ident -> State Env (Maybe Ident)
lookupMethod i = do
  st <- get
  let mcxt = (metNames st) in
    return $ getIdent i mcxt

--helper to lookupMethod
getMethod :: Ident -> Cxt -> Maybe Ident
getMethod i [] = Nothing
getMethod i (n:ns) =
       case Map.lookup i n of
          Nothing -> getMethod i ns
          ident -> ident

--lookup but if metehod does not exist, just return original name.
lookupMetInScope :: Ident -> [Map Ident Ident] -> Maybe Ident
lookupMetInScope i [] = Just i
lookupMetInScope i (m:ms) = do
  case Map.lookup i m of
    Nothing -> lookupMetInScope i ms
    name -> name


--jämför träden så att de returnerar nothing om den nya är == med gamla
execute :: CompilationUnit -> Maybe CompilationUnit
execute cu =
  let ast = evalState (rename cu) newEnv in
  case cu == ast of
    True -> Nothing
    False -> Just $ ast

--Renames all class names, method names, formalparams and method bodies
rename :: CompilationUnit -> State Env CompilationUnit
rename hcu@(HoleCompilationUnit _) = return hcu
-- TODO: handle import statements!
rename (CompilationUnit is typeDecls) =
    mapM renameClassName typeDecls >>= \td ->
    mapM renameAllMethodNames td >>= \td' ->
    CompilationUnit is <$> mapM renameClass td'

--Renames all FormalParams and, MethodBodies in a Class in a Context
--Does not rename ClassName, MethodName
renameClass :: TypeDecl -> State Env TypeDecl
renameClass htd@(HoleTypeDecl _) = return htd
renameClass (ClassTypeDecl ctd) =
  case ctd of
    (ClassDecl ident (ClassBody decls)) -> do
      newContext
      newMethodCxt
      decls' <- mapM renameMethod decls
      exitContext
      exitMethodCxt
      return (ClassTypeDecl (ClassDecl ident (ClassBody decls')))
    holeClassDecl -> return $ ClassTypeDecl holeClassDecl

--Renames a class to a new (Unique) Ident
renameClassName :: TypeDecl-> State Env TypeDecl
renameClassName htd@(HoleTypeDecl _ ) = return htd
renameClassName (ClassTypeDecl ctd) =
  case ctd of
    (ClassDecl ident body) ->
      newClassName ident >>= \name ->
      return (ClassTypeDecl (ClassDecl name body))
    holeClassDecl -> return $ ClassTypeDecl holeClassDecl

--Renamses all method names in a Class to a new Ident
renameAllMethodNames :: TypeDecl -> State Env TypeDecl
renameAllMethodNames htd@(HoleTypeDecl _ ) = return htd
renameAllMethodNames (ClassTypeDecl ctd) =
  case ctd of
    (ClassDecl ident (ClassBody decls)) ->
       mapM renameMethodName decls >>= \ds ->
       return (ClassTypeDecl (ClassDecl ident (ClassBody ds)))
    holeClassDecl -> return $ ClassTypeDecl holeClassDecl

--Renames a method to a new (Unique) Ident
renameMethodName :: Decl -> State Env Decl
renameMethodName hd@(HoleDecl _) = return hd
renameMethodName (MemberDecl md) =
  case md of
    (MethodDecl mType ident formalParams block) ->
      newMethodName ident >>= \name ->
      return (MemberDecl $ MethodDecl mType name formalParams block)
    holeMethodDecl -> return $ MemberDecl holeMethodDecl

--Renames FormalParams and MethodBody (Block) in a method context
renameMethod :: Decl -> State Env Decl
renameMethod hd@(HoleDecl _) = return hd
renameMethod (MemberDecl (MethodDecl mType ident formalParams block)) = do
  newContext
  fp <- mapM renameFormalParam formalParams
  b <- renameBlock block
  exitContext
  return (MemberDecl (MethodDecl mType ident fp b))
renameMethod hd@(MemberDecl _) = return hd

--Renames the Formal Parameters
renameFormalParam :: FormalParam -> State Env FormalParam
renameFormalParam (FormalParam vmType varDeclId) = do
  case varDeclId of
    (VarDId ident) ->
      newVarName ident >>= \name ->
      return (FormalParam vmType (VarDId name))
    (VarDArr ident i) ->
      newVarName ident >>= \name ->
      return (FormalParam vmType (VarDArr name i))

--Renames a Stetment
renameStatement :: Stmt -> State Env Stmt
renameStatement statement = do
  case statement of
    SEmpty -> return SEmpty
    (SBlock block) -> do
      newContext
      block' <- SBlock <$> renameBlock block
      exitContext
      return block'
    (SExpr expr)        -> SExpr <$> renameExpression expr
    (SVars typedVVDecl) -> SVars <$> renameTypedVVDecl typedVVDecl
    (SReturn expr)      -> SReturn <$> renameExpression expr
    (SVReturn)          -> return statement
    (SIf expr stmt)     ->
      SIf
      <$> renameExpression expr
      <*> renameStatement stmt
    (SIfElse expr stmt1 stmt2) ->
      SIfElse
      <$> renameExpression expr
      <*> renameStatement stmt1
      <*> renameStatement stmt2
    (SWhile expr stmt)       ->
      SWhile
      <$> renameExpression expr
      <*> renameStatement stmt
    (SDo expr stmt)          ->
      SDo
      <$> renameExpression expr
      <*> renameStatement stmt
    (SForB mForInit mExpr mExprs stmt) ->
      SForB
      <$> maybe (return Nothing) ((fmap Just)
        . renameForInit) mForInit
      <*> maybe (return Nothing) ((fmap Just)
        . renameExpression) mExpr
      <*> maybe (return Nothing) ((fmap Just)
        . (mapM renameExpression)) mExprs
      <*> renameStatement stmt
    (SForE vMType ident expr stmt) ->
      SForE vMType
      <$> newVarName ident
      <*> renameExpression expr
      <*> renameStatement stmt
    (SContinue) -> return statement
    (SBreak)    -> return statement
    (SSwitch expr switchBlocks) ->
      SSwitch
      <$> renameExpression expr
      <*> mapM renameSwitch switchBlocks
    (HoleStmt i) -> return (HoleStmt i)

--Renames a Block
renameBlock :: Block -> State Env Block
renameBlock block  = case block of
  (Block ss) -> Block <$> mapM renameStatement ss
  holeBlock  -> return holeBlock

--Renames an Expression
renameExpression :: Expr -> State Env Expr
renameExpression expression =
  case expression of
    (ELit literal) -> return (ELit literal)
    (EVar lValue) -> EVar <$> renameLValue lValue
    (ECast t expr) -> ECast t <$> renameExpression expr
    (ECond expr1 expr2 expr3) ->
      ECond
      <$> renameExpression expr1
      <*> renameExpression expr2
      <*> renameExpression expr3
    (EAssign lValue expr) ->
      EAssign
      <$> renameLValue lValue
      <*> renameExpression expr
    (EOAssign lValue numOp expr) ->
      renameLValue lValue >>= \v ->
      renameExpression expr >>= \e ->
      return (EOAssign v numOp e)
    (ENum numOp expr1 expr2) ->
      ENum numOp
      <$> renameExpression expr1
      <*> renameExpression expr2
    (ECmp cmpOp expr1 expr2) ->
      ECmp cmpOp
      <$> renameExpression expr1
      <*> renameExpression expr2
    (ELog logOp expr1 expr2) ->
      ELog logOp
      <$> renameExpression expr1
      <*> renameExpression expr2
    (ENot expr) -> ENot <$> renameExpression expr
    (EStep stepOp expr) -> EStep stepOp <$> renameExpression expr
    (EBCompl  expr) ->
      EBCompl <$> renameExpression expr
    (EPlus    expr) ->
      EPlus <$> renameExpression expr
    (EMinus   expr)->
      EMinus <$> renameExpression expr
    (EMApp (Name names) exprs) -> do
      st <- get
      let mcxs = (metNames st)
      case names of
        [x]      -> mapM renameExpression exprs >>= \es ->
                    return $ EMApp (Name [(singleMethod x mcxs)]) es
        (x:xs)   -> return(last names) >>= \metName ->
                    mapM findVarName (init names) >>= \vars ->
                    return (fromJust(lookupMetInScope metName mcxs)) >>= \met ->
                    mapM renameExpression exprs >>= \es ->
                    return (Name (vars ++ [met])) >>= \rNames ->
                    return (EMApp rNames es)
    (EArrNew  t exprs i) ->
      mapM renameExpression exprs >>= \es -> return (EArrNew t es i)
    (EArrNewI t i (ArrayInit arrayInit)) ->
      mapM renameVarInit arrayInit >>= \ai ->
        return (EArrNewI t i (ArrayInit ai))
    (ESysOut  expr) -> ESysOut <$> renameExpression expr
    holeExpr -> return holeExpr

--When it's just a single method call
singleMethod :: Ident -> MCxt -> Ident
singleMethod x mcxs =
  fromJust(lookupMetInScope x mcxs)

--Renames a lvalue.
renameLValue :: LValue -> State Env LValue
renameLValue lValue = case lValue of
  (LVName (Name [ident])) -> singVar <$> newVarName ident
  (LVName x) -> pure lValue -- TODO: Fix logic for this case.
  (LVArray expr exprs) ->
    LVArray
    <$> renameExpression expr
    <*> mapM renameExpression exprs
  holeLValue -> return holeLValue

--Renames a For initialization
renameForInit :: ForInit -> State Env ForInit
renameForInit forInit = case forInit of
  (FIVars typedVVDecl) -> FIVars <$> renameTypedVVDecl typedVVDecl
  (FIExprs exprs) -> FIExprs <$> mapM renameExpression exprs
  holeForInit -> return holeForInit

--Renames a Typed VV Decleration
renameTypedVVDecl :: TypedVVDecl -> State Env TypedVVDecl
renameTypedVVDecl typedVVDecl = case typedVVDecl of
  (TypedVVDecl vMType varDecls) ->
    TypedVVDecl vMType <$> mapM renameVarDecl varDecls
  holeTypedVVDecl -> return holeTypedVVDecl

--Renames a Variable Declaration
renameVarDecl :: VarDecl -> State Env VarDecl
renameVarDecl varDecl = case varDecl of
  (VarDecl varDeclId mVarInit) ->
    maybe (return Nothing) ((fmap Just) . renameVarInit) mVarInit >>= \mvi ->
    renameVarDleclId varDeclId >>= \vdi -> return (VarDecl vdi mvi)
  holeVarDecl -> return holeVarDecl

--Renames a Variable Initialization
renameVarInit :: VarInit -> State Env VarInit
renameVarInit varInit =
  case varInit of
    (InitExpr expr) -> InitExpr <$> renameExpression expr
    (InitArr  (ArrayInit arrayInit)) ->
      InitArr
      . ArrayInit
      <$> mapM renameVarInit arrayInit
    holeVarInit -> return holeVarInit

--Renames a Variable Declaration Id
renameVarDleclId :: VarDeclId -> State Env VarDeclId
renameVarDleclId varDeclId =
  case varDeclId of
    (VarDId  ident) -> VarDId <$> newVarName ident
    (VarDArr ident i) ->
      newVarName ident >>= \new ->
      return (VarDArr new i)
    holeVarDeclId -> return holeVarDeclId

--Renames a Switch Block / Statement
renameSwitch :: SwitchBlock -> State Env SwitchBlock
renameSwitch hsb@(HoleSwitchBlock i) = return hsb
renameSwitch (SwitchBlock label (Block block)) =
  case label of
  (SwitchCase expr) ->
    renameExpression expr >>= \e ->
    mapM renameStatement block >>=
      return . SwitchBlock (SwitchCase e) . Block
  Default ->
    SwitchBlock Default . Block <$> mapM renameStatement block
  holeSwitchLabel -> SwitchBlock holeSwitchLabel . Block <$> mapM renameStatement block
