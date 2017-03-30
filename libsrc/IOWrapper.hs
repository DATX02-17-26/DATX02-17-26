module IOWrapper where

import Control.Monad.State
import CoreS.AST
import Data.List (delete, elem)
import Control.Lens
import CoreS.Parse
import AlphaR

type IdDecl = (Ident, Decl)

--Wraps the decleration that is going to be tested and includes the corresponding
-- declerations that where in that class
wrapDecl :: IdDecl -> [IdDecl] -> CompilationUnit
wrapDecl (ident@(Ident id), decl) idDecls =
  CompilationUnit
  $ [ClassTypeDecl
  $ ClassDecl (Ident (id ++ (declName decl)))
  $ ClassBody $ [(wrapMain decl)] ++ (map snd $ filter ((==ident) . fst) idDecls)
  ]

wrap :: CompilationUnit -> [CompilationUnit] -> [(CompilationUnit, [CompilationUnit])]
wrap s m = match student models
              where
                student = getIdDecls $ removeMain s
                models = map getIdDecls $ map removeMain m

match :: [IdDecl] -> [[IdDecl]] -> [(CompilationUnit, [CompilationUnit])]
match student@(s:xs) models = (wrapDecl s student , filter models) : match xs models
  where
    filter [] = []
    filter (m:ms) =  wrapAll (filterMethods s m) m ++ filter ms

--removes all (ident, decl) that are not equal to decl
filterMethods :: IdDecl -> [IdDecl] -> [IdDecl]
filterMethods (_, student) models = filter f models
  where f (_, decl) = isSameMethod student decl

--Makes a main method of every decl in the first argument and adds the decls of the second
wrapAll :: [IdDecl] -> [IdDecl] -> [CompilationUnit]
wrapAll [] _ = []
wrapAll (t:ts) decls = wrapDecl t decls : wrapAll ts decls

removeMain :: CompilationUnit -> CompilationUnit
removeMain (CompilationUnit typeDecls) =
  CompilationUnit . putClassBodys typeDecls $ map filterMain $ map getClassBody typeDecls

filterMain :: ClassBody -> ClassBody
filterMain (ClassBody decls) = (ClassBody $ filter isNotMain decls)

isNotMain :: Decl -> Bool
isNotMain decl =
  case decl of
    (MemberDecl(MethodDecl _ (Ident "main") _ _ )) -> False
    _                                            -> True

wrapMain :: Decl -> Decl
wrapMain decl@(MemberDecl(MethodDecl _ _ formalParams _ )) =
  (MemberDecl (MethodDecl Nothing (Ident "main")
  [(FormalParam (VMType VMNormal (ArrayT StringT)) (VarDId (Ident "args")))]
    (Block [ (SExpr (ESysOut (EMApp (Name [Ident (declName decl)]) (getArgs formalParams 0))))
  ])))

declName :: Decl -> String
declName (MemberDecl(MethodDecl _ (Ident ident) _ _ )) = ident

getArgs :: [FormalParam] -> Int -> [Expr]
getArgs [] _ = []
getArgs (p@(FormalParam vmType varDeclId):ps) pos =
    (EVar (LVName (Ident ((cast $ makeType p) ++ "args[" ++ (show pos) ++ "])"))))
    : (getArgs ps (pos+1))

cast :: Type -> String
cast t = case t of
  (PrimT primType) -> castPrim primType
  StringT -> ""
  ArrayT t' -> cast t'

castPrim :: PrimType -> String
castPrim p = case p of
  BoolT -> "Boolean.parseBoolean("
  ByteT -> "Byte.valueOf("
  ShortT -> "Short.parseShort("
  IntT -> "Integer.parseInt("
  LongT -> "Long.parseLong("
  CharT -> "Integer.parseInt("
  FloatT -> "Float.parseFloat("
  DoubleT -> "Double.parseDouble("

--I assume that 2 methods does the same thing if they have the same:
--Return Type and, number of and Types of formal parameters
isSameMethod :: Decl -> Decl -> Bool
isSameMethod (MemberDecl (MethodDecl sType sIdent sFormalParams sBlock))
             (MemberDecl (MethodDecl mType mIdent mFormalParams mBlock)) =
      if sType == mType then
        compareFormalParams sFormalParams mFormalParams
      else False

compareFormalParams :: [FormalParam] -> [FormalParam]-> Bool
compareFormalParams student model =
  compareTypes (map makeType student) (map makeType model)

makeType :: FormalParam -> Type
makeType (FormalParam (VMType _ t) _ ) = t

compareTypes :: [Type] -> [Type] -> Bool
comparetypes [] [] = True
compareTypes student@(x:xs) model =
  if x `elem` model then
    compareTypes xs (delete x model)
  else False

getIdDecls :: CompilationUnit -> [(Ident, Decl)]
getIdDecls(CompilationUnit typeDecls) = concat $ map fromTDtoDecl typeDecls

fromTDtoDecl :: TypeDecl -> [(Ident, Decl)]
fromTDtoDecl(ClassTypeDecl (ClassDecl ident (ClassBody decls))) =
  map zipIdent decls
  where zipIdent decl = (ident,decl)

getTypeDecls :: CompilationUnit -> [TypeDecl]
getTypeDecls (CompilationUnit typeDecls) = typeDecls

getClassBody :: TypeDecl -> ClassBody
getClassBody (ClassTypeDecl (ClassDecl ident classBody)) = classBody

putClassBodys :: [TypeDecl] -> [ClassBody] -> [TypeDecl]
putClassBodys (t:ts) (c:cs) = putClassBody t c : putClassBodys ts cs

putClassBody :: TypeDecl -> ClassBody -> TypeDecl
putClassBody (ClassTypeDecl (ClassDecl ident _)) cb  = (ClassTypeDecl (ClassDecl ident cb))
