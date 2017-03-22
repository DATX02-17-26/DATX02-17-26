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

wrapMain :: Decl -> Decl
wrapMain decl = (MemberDecl (MethodDecl Nothing (Ident "main")
  [(FormalParam (VMType VMNormal (ArrayT StringT)) (VarDId (Ident "args")))]
  (Block [decl])))

declName :: Decl -> String
declName (MemberDecl(MethodDecl _ (Ident ident) _ _ )) = ident

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




