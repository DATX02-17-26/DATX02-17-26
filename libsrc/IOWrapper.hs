module IOWrapper where

import Control.Monad.State
import CoreS.AST
import Data.List (delete, elem)
import Control.Lens
import CoreS.Parse
import AlphaR

main :: IO()
main = do
  s <- readFile "C:/Workspace/DATX02-17-26/Test/AlphaTest/Good1.java"
 -- s' <- readFile "C:/Workspace/DATX02-17-26/Test/AlphaTest/Good1-1.java"
  let ast = parseConv s
  --let ast2= parseConv s'
  student <- either undefined pure ast
  --model  :: CompilationUnit <- either undefined pure ast2
  print $ wrap (student :: CompilationUnit)

wrap cu = cu

fromCUtoCD (CompilationUnit typeDecls) = map fromTDtoDecl typeDecls

fromTDtoDecl(ClassTypeDecl (ClassDecl ident (ClassBody decls))) =
  decls


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






