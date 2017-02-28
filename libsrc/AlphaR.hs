module AlphaR where 

import Control.Monad
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
newClassName :: Ident -> State Env Ident
newClassName old = do
   modify (\s -> s{cName = (cName s) + 1}) 
   st <- get 
   name  <- return (cName st)
   let new = (Ident $ "Class" ++ show name)
   addIdent new old

--create new method name
newMethodName :: Ident -> State Env Ident
newMethodName old = do
   modify (\s -> s{mName = (mName s) + 1}) 
   st <- get 
   name  <- return (mName st)
   let new = (Ident $ "method" ++ show name)
   addIdent new old

--create new variable name
newVarName :: Ident -> State Env Ident
newVarName old = do
   modify (\s -> s{vName = (vName s) + 1}) 
   st    <- get 
   name  <- return (vName st)
   let new =  (Ident $ "var" ++ show name)
   addIdent new old

--add a Ident to Env return the new ident
addIdent :: Ident -> Ident -> State Env Ident
addIdent new old = do
  st <- get
  let (n:ns) = (names st)
  modify(\s -> s{names = (Map.insert new old n):ns})
  return new

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

execute :: CompilationUnit -> Maybe CompilationUnit
execute cu = Just $ evalState (rename cu) newEnv

--Renames all class names, method names, formalparams and method bodies
rename :: CompilationUnit -> State Env CompilationUnit
rename (CompilationUnit typeDecls) =
    mapM renameClassName typeDecls >>= \td -> 
    mapM renameAllMethodNames td >>= \td' ->
    CompilationUnit <$> mapM renameClass td' 
   

--Renames all FormalParams and, MethodBodies in a Class in a Context
--Does not rename ClassName, MethodName
renameClass :: TypeDecl -> State Env TypeDecl
renameClass (ClassTypeDecl (ClassDecl ident (ClassBody decls))) = do
  newContext
  decls' <- mapM renameMethod decls
  exitContext
  return (ClassTypeDecl (ClassDecl ident (ClassBody decls'))) 


--Renames a class to a new (Unique) Ident
renameClassName :: TypeDecl-> State Env TypeDecl
renameClassName (ClassTypeDecl (ClassDecl ident body)) =
      newClassName ident >>= \name ->
      return (ClassTypeDecl (ClassDecl name body))

--Renamses all method names in a Class to a new Ident
renameAllMethodNames :: TypeDecl -> State Env TypeDecl
renameAllMethodNames (ClassTypeDecl (ClassDecl ident (ClassBody decls))) =
   mapM renameMethodName decls >>= \ds -> 
   return (ClassTypeDecl (ClassDecl ident (ClassBody ds)))

--Renames a method to a new (Unique) Ident
renameMethodName :: Decl -> State Env Decl
renameMethodName (MemberDecl
                 (MethodDecl mType ident formalParams block)) = 
          newMethodName ident >>= \name ->
          return (MemberDecl $ MethodDecl mType name formalParams block)

--Renames FormalParams and MethodBody (Block) in a method context
renameMethod :: Decl -> State Env Decl
renameMethod (MemberDecl (MethodDecl mType ident formalParams block)) = do
  newContext
  fp <- mapM renameFormalParam formalParams
  b <- renameBlock block
  exitContext
  return (MemberDecl (MethodDecl mType ident fp b))


renameFormalParam :: FormalParam -> State Env FormalParam
renameFormalParam (FormalParam vmType varDeclId) = do
  case varDeclId of
    (VarDId ident) ->
      newVarName ident >>= \name ->
      return (FormalParam vmType (VarDId name))
    (VarDArr ident i) ->
      newVarName ident >>= \name ->
      return (FormalParam vmType (VarDArr name i))


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


renameBlock :: Block -> State Env Block
renameBlock (Block ss) =
  Block <$> mapM renameStatement ss 


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
    (EMApp name exprs) ->
      EMApp name <$> mapM renameExpression exprs
    (EArrNew  t exprs i) ->
      mapM renameExpression exprs >>= \es -> return (EArrNew t es i)
    (EArrNewI t i (ArrayInit arrayInit)) -> 
      mapM renameVarInit arrayInit >>= \ai -> 
        return (EArrNewI t i (ArrayInit ai))
    (ESysOut  expr) -> ESysOut <$> renameExpression expr


renameLValue :: LValue -> State Env LValue
renameLValue lValue = case lValue of
  (LVName ident) -> LVName <$> newVarName ident
  (LVArray expr exprs) ->
    LVArray
    <$> renameExpression expr
    <*> mapM renameExpression exprs
      

renameForInit :: ForInit -> State Env ForInit
renameForInit forInit = case forInit of
  (FIVars typedVVDecl) -> FIVars <$> renameTypedVVDecl typedVVDecl
  (FIExprs exprs) -> FIExprs <$> mapM renameExpression exprs


renameTypedVVDecl :: TypedVVDecl -> State Env TypedVVDecl
renameTypedVVDecl (TypedVVDecl vMType varDecls) = 
  TypedVVDecl vMType <$> mapM renameVarDecl varDecls


renameVarDecl :: VarDecl -> State Env VarDecl
renameVarDecl (VarDecl varDeclId mVarInit) =
   maybe (return Nothing) ((fmap Just) . renameVarInit) mVarInit >>= \mvi ->
   renameVarDleclId varDeclId >>= \vdi -> return (VarDecl vdi mvi)

int x = 0;
{
int x = x +1;
}


renameVarInit :: VarInit -> State Env VarInit
renameVarInit varInit =
  case varInit of
    (InitExpr expr) -> InitExpr <$> renameExpression expr
    (InitArr  (ArrayInit arrayInit)) -> 
      InitArr 
      . ArrayInit 
      <$> mapM renameVarInit arrayInit


renameVarDleclId :: VarDeclId -> State Env VarDeclId
renameVarDleclId varDeclId =
  case varDeclId of 
    (VarDId  ident) -> VarDId <$> newVarName ident
    (VarDArr ident i) -> 
      newVarName ident >>= \new -> 
      return (VarDArr new i)


renameSwitch :: SwitchBlock -> State Env SwitchBlock
renameSwitch (SwitchBlock label (Block block)) = 
  case label of
  (SwitchCase expr) ->
    renameExpression expr >>= \e -> 
    mapM renameStatement block >>= 
      return . SwitchBlock (SwitchCase e) . Block  
  Default ->
    SwitchBlock Default . Block <$> mapM renameStatement block

