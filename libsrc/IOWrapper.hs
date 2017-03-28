module IOWrapper where

import Control.Monad.State
import CoreS.AST
import Data.List (delete, elem)
import Control.Lens
import CoreS.Parse
import AlphaR

wrapDecl ((Ident id), decl) =
  CompilationUnit
  $ [ClassTypeDecl
  $ ClassDecl (Ident (id ++ (declName decl)))
  $ ClassBody [decl]
  ]

wrap :: CompilationUnit -> [CompilationUnit] -> (CompilationUnit, [CompilationUnit])
wrap = undefined

wrapMain ::Decl -> Decl
wrapMain decl@(MemberDecl(MethodDecl _ _ formalParams _ )) = (MemberDecl (MethodDecl Nothing (Ident "main")
  [(FormalParam (VMType VMNormal (ArrayT StringT)) (VarDId (Ident "args")))]
  (Block [
    (SExpr (ESysOut (EMApp (Name [Ident (declName decl)]) (getArgs formalParams 0))))
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
  _ -> undefined

castPrim :: PrimType -> String
castPrim p = case p of
  BoolT -> "Boolean.parseBoolean("
  ByteT -> "Byte.valueOf("
  ShortT -> "Short.parseShort("
  IntT -> "Integer.parseInt("
  LongT -> "Long.parseLong("
  CharT -> undefined
  FloatT -> "Float.parseFloat("
  DoubleT -> "Double.parseDouble("

filterMethods :: Decl -> [(Ident, Decl)] -> [(Ident, Decl)]
filterMethods student models = filter f models
  where f (_, decl) = isSameMethod student decl


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




