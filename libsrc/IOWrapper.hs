module IOWrapper where

import Control.Monad.State
import CoreS.AST
import Data.List (delete, elem)

data File = File {
  code :: [Statement]
  }

data Statement = String

print :: Statement -> State File ()
print s = modify (\st -> st{code = s : code st})

newFile :: File
newFile = File { code = []}

wrap :: CompilationUnit -> CompilationUnit -> IO ()
wrap student model =

--I assume that 2 methods does the same thing if they have the same:
--Return Type and, number of and Types of formal parameters
isSameMethod :: MemberDecl -> MemberDecl -> Bool
isSameMethod student@(MethodDecl sType sIdent sFormalParams sBlock)
               model@(MethodDecl mType mIdent mFormalParams mBlock) =
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






