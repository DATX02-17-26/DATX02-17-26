module IOWrapper (
wrap
, makeType
)where

import Control.Monad.State
import CoreS.AST
import Data.List (delete, elem)
import Control.Lens
import CoreS.Parse
import AlphaR
import System.FilePath
import System.IO
import CoreS.ConvBack

type IdDecl = (Ident, Decl)

-- | The context in which we are investigating a student solution
data SolutionContext a = Ctx { studentSolution :: a
                             , modelSolutions  :: [a]
                             }

--Wraps the decleration that is going to be tested and includes the corresponding
-- declerations that where in that class
wrapDecl :: IdDecl -> [IdDecl] -> CompilationUnit
wrapDecl (ident@(Ident id), decl) idDecls =
  CompilationUnit
  $ [ClassTypeDecl
  $ ClassDecl (Ident (id ++ (declName decl)))
  $ ClassBody $ [(wrapMain decl)] ++ (map snd $ filter ((==ident) . fst) idDecls)
  ]

wrap :: CompilationUnit -> [CompilationUnit] -> [(Decl, (CompilationUnit, [CompilationUnit]))]
wrap s m = match student models
              where
                student = getIdDecls $ removeMain s
                models = map getIdDecls $ map removeMain m

match :: [IdDecl] -> [[IdDecl]] -> [(Decl, (CompilationUnit, [CompilationUnit]))]
match student@(s:xs) models = (snd s, (wrapDecl s student , filter models)) : match xs models
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

printWrappedSolutions :: FilePath -> SolutionContext CompilationUnit -> IO [(Decl,(FilePath, FilePath))]
printWrappedSolutions fp ctx = printSolutions
  where
    student = studentSolution ctx
    models = modelSolutions ctx
    sol = wrap student models
    wStud = map (fst . snd) sol
    wMods = map (snd . snd) sol
    wMethods = [(className stud, className <$> mods) | stud <- wStud, mods <- wMods]
    convert = [(convSol stud, convSol <$> mods) | stud <- wStud, mods <- wMods]
    filePaths = makeFP fp wMethods
    printSolutions = do
      printStudMods filePaths wMethods convert
      return (zip (map fst sol) filePaths)

-- | Zip together two SolutionContext's
zipContexts :: SolutionContext a -> SolutionContext b -> SolutionContext (a, b)
zipContexts (Ctx a as) (Ctx b bs) = Ctx (a, b) (zip as bs)

printStudMods :: [(FilePath,FilePath)] -> [(String,[String])]-> [(String,[String])] -> IO ()
printStudMods fp names solutions =  printSol fp names solutions
       where
        printSol [][][] = return ()
        printSol (fp:fs) ((studName,modNames):ns) ((studClass,modClasses):xs) = do
          printStudent (fst fp) studClass
          printModels (snd fp) modNames modClasses
          printSol fs ns xs

makeFP :: FilePath -> [(String,[String])] -> [(FilePath,FilePath)]
makeFP _ [] = []
makeFP fp ((studName,_):ns) =
  ((fp </> "students" </> studName ++ ".java"),(fp </> "models" </> studName)):makeFP fp ns


className (CompilationUnit td) = tdName $ head td
  where tdName (ClassTypeDecl (ClassDecl (Ident i) _)) = i

convSol :: CompilationUnit -> String
convSol sol = case prettyCore sol of
  Left _ -> "Couldn't parse solution"
  Right b -> b

printModels :: FilePath -> [String] -> [String] -> IO ()
printModels fp name body = printModel name body
  where
    printModel [] [] = return ()
    printModel (name:ns) (body:bs) = do
      writeFile (fp </> name ++ ".java") body
      printModel ns bs

printStudent :: FilePath -> String -> IO ()
printStudent fp body = writeFile fp body
