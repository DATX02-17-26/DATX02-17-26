module MethodSort where

import Data.List
import CoreS.AST
import NormalizationStrategies (makeRule, NormalizationRule)

execute :: CompilationUnit -> Maybe CompilationUnit
execute cu = case cu of
  CompilationUnit tds ->
    let normCU = CompilationUnit $ intoClassTypeDecl <$> tds
    in if cu == normCU then Nothing else Just normCU
  hole -> Nothing

--start at classBody, go into ClassDecl
intoClassTypeDecl :: TypeDecl -> TypeDecl
intoClassTypeDecl ctd = case ctd of
  ClassTypeDecl cd -> ClassTypeDecl (intoClassDecl cd)
  hole             -> hole
  --ClassTypeDecl $ ClassDecl ident $ ClassBody $ intoMethodbody <$> decls

--ClassDecl go into ClassBody
intoClassDecl :: ClassDecl -> ClassDecl
intoClassDecl cd = case cd of
  (ClassDecl ident cb) -> ClassDecl ident (intoClassBody cb)
  hole                 -> hole

--ClassBody go into Decl
intoClassBody :: ClassBody -> ClassBody
intoClassBody cb = case cb of
  ClassBody decls -> ClassBody (sortRet decls)
  hole            -> hole

sortRetType :: Decl -> Decl -> Ordering
sortRetType d1 d2
  | getType d1 < getType d2 = Prelude.LT
  | getType d1 > getType d2 = Prelude.GT
  | getType d1 == getType d2 = Prelude.EQ


sortRet :: [Decl] -> [Decl]
sortRet decls = sortBy sortRetType decls
{-}
--Slow but works. Can be made prettier. Sorts methods according to return type
sortRet :: [Decl] -> [Decl]
sortRet decls = do
  let bools = filter (\t -> getType t == Just(PrimT BoolT)) decls
  let bytes = filter (\t -> getType t == Just(PrimT ByteT)) decls
  let shorts = filter (\t -> getType t == Just(PrimT ShortT)) decls
  let ints = filter (\t -> getType t == Just(PrimT IntT)) decls
  let longs = filter (\t -> getType t == Just(PrimT LongT)) decls
  let chars = filter (\t -> getType t == Just(PrimT CharT)) decls
  let floats = filter (\t -> getType t == Just(PrimT FloatT)) decls
  let dbls = filter (\t -> getType t == Just(PrimT DoubleT)) decls
  let strings = filter (\t -> getType t == Just StringT) decls
  let arrs = filter isArr decls
  let voids = filter (\t -> getType t == Nothing) decls
  bools ++ bytes ++ shorts ++ ints ++ longs ++ chars
     ++ floats ++ dbls ++ strings ++ arrs ++ voids
  --case decls of
  --(m:ms) -> case m of
  --  MemberDecl (MethodDecl (Just (PrimT BoolT)) i fp b)  -> sortRet ms m:sorted
  --  MemberDecl (MethodDecl (Just (PrimT ByteT)) i fp b)  -> m:(sortRet ms)
  --hole          -> hole
-}
--check type
getMayType :: Decl -> Maybe Type
getMayType md = case md of
  MemberDecl (MethodDecl (Just t) _ _ _) -> Just t
  MemberDecl (MethodDecl Nothing _ _ _)  -> Nothing

  --check type
getType :: Decl -> Type
getType md = case md of
  MemberDecl (MethodDecl (Just t) _ _ _) -> t
  MemberDecl (MethodDecl Nothing _ _ _)  -> NullT


--if array
isArr :: Decl -> Bool
isArr md = case md of
  MemberDecl (MethodDecl (Just (ArrayT t)) _ _ _) -> True
  _                                               -> False
