{- DATX02-17-26, automated assessment of imperative programs.
 - Copyright, 2017, see AUTHORS.md.
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-# LANGUAGE LambdaCase, TupleSections #-}

module CoreS.Convert where

import Safe (headMay)
import Prelude hiding (LT, GT, EQ)

import qualified Language.Java.Syntax as S
import CoreS.AST

--------------------------------------------------------------------------------
-- Conversion:
--------------------------------------------------------------------------------

type ConvErr = String
type CConv a = Either ConvErr a

unimpl :: Show x => x -> CConv y
unimpl x = Left $ show x ++ " is not supported yet!"

--------------------------------------------------------------------------------
-- Conversion, Types:
--------------------------------------------------------------------------------

convPTyp :: S.PrimType -> PrimType
convPTyp = \case
  S.BooleanT -> BoolT
  S.ByteT    -> ByteT
  S.ShortT   -> ShortT
  S.IntT     -> IntT
  S.LongT    -> LongT
  S.CharT    -> CharT
  S.FloatT   -> FloatT
  S.DoubleT  -> DoubleT

strTPrefix = S.ClassType $ ((, []) . S.Ident) <$> ["java", "lang", "String"]
strT       = S.ClassType $ ((, []) . S.Ident) <$> ["String"]

convRTyp :: S.RefType -> CConv Type
convRTyp = \case
  S.ClassRefType ct |
    ct `elem` [strT, strTPrefix] -> pure StringT
  S.ArrayType    t               -> ArrayT <$> convTyp t
  x                              -> unimpl x

convTyp :: S.Type -> CConv Type
convTyp = \case
  S.PrimType t -> pure $ PrimType $ convPTyp t
  S.RefType  t -> convRTyp t

convId :: S.Ident -> Ident
convId (S.Ident i) = Ident i

convName :: S.Name -> Name
convName (S.Name is) = Name $ convId <$> is

--------------------------------------------------------------------------------
-- Conversion, Expression:
--------------------------------------------------------------------------------

u = undefined

convLit :: S.Literal -> CConv Expr
convLit = pure . ELit . \case
  S.Int     i -> Int     i
  S.Word    w -> Word    w
  S.Float   f -> Float   f
  S.Double  d -> Double  d
  S.Boolean b -> Boolean b
  S.Char    c -> Char    c
  S.String  s -> String  s
  S.Null      -> Null

convUna :: S.Exp -> (Expr -> y) -> CConv y
convUna e ctor = ctor <$> convExp e

convBin :: S.Exp -> S.Exp -> (Expr -> Expr -> y) -> CConv y
convBin l r ctor = ctor <$> convExp l <*> convExp r

convNum :: S.Exp -> S.Exp -> NumOp -> CConv Expr
convNum l r op = convBin l r $ ENum op

convCmp :: S.Exp -> S.Exp -> CmpOp -> CConv Expr
convCmp l r op = convBin l r $ ECmp op

convLog :: S.Exp -> S.Exp -> LogOp -> CConv Expr
convLog l r op = convBin l r $ ELog op

convBOp :: S.Exp -> S.Exp -> S.Op -> CConv Expr
convBOp l r = \case
  S.Add     -> convNum l r Add
  S.Sub     -> convNum l r Sub
  S.Mult    -> convNum l r Mul 
  S.Div     -> convNum l r Div
  S.Rem     -> convNum l r Rem  
  S.And     -> convNum l r And
  S.Xor     -> convNum l r Xor
  S.Or      -> convNum l r Or
  S.LShift  -> convNum l r LShift
  S.RShift  -> convNum l r RShift
  S.RRShift -> convNum l r RRShift
  S.LThan   -> convCmp l r LT
  S.GThan   -> convCmp l r GT
  S.LThanE  -> convCmp l r LE
  S.GThanE  -> convCmp l r GE
  S.Equal   -> convCmp l r EQ
  S.NotEq   -> convCmp l r NE
  S.CAnd    -> convLog l r LAnd
  S.COr     -> convLog l r LOr

convStep :: S.Exp -> StepOp -> CConv Expr
convStep e op = convUna e $ EStep op

convOne :: Show a => [a] -> CConv a
convOne = \case [x] -> pure x
                xs  -> unimpl xs

convExpN :: S.Name -> CConv Expr
convExpN n@(S.Name ns) = EVar . LVName . convId <$> convOne ns

convArrAcc :: S.ArrayIndex -> CConv Expr
convArrAcc (S.ArrayIndex e eis) =
  EVar <$> (LVArray <$> convExp e <*> mapM convExp eis)

convArrCreate :: S.Type -> [S.Exp] -> Int -> CConv Expr
convArrCreate t ls lex = EArrNew <$> convTyp t
                                 <*> mapM convExp ls
                                 <*> pure (toInteger lex)

convVarInit :: S.VarInit -> CConv VarInit
convVarInit = \case
  S.InitExp    e -> InitExpr <$> convExp e
  S.InitArray ai -> InitArr  <$> convArrInit ai

convArrInit :: S.ArrayInit -> CConv ArrayInit
convArrInit (S.ArrayInit ai) = ArrayInit <$> mapM convVarInit ai

convArrCreateI :: S.Type -> Int -> S.ArrayInit -> CConv Expr
convArrCreateI t dl ai = EArrNewI <$> convTyp t
                                  <*> pure (toInteger dl)
                                  <*> convArrInit ai

printLn :: Name
printLn = Name $ Ident <$> ["System", "out", "println"]

convApp :: S.MethodInvocation -> CConv Expr
convApp = \case
  S.MethodCall n args -> do
    args' <- mapM convExp args
    case convName n of
      m | m == printLn -> ESysOut <$> maybe (unimpl args') pure (headMay args')
        | otherwise    -> pure $ EMApp m args'
  x -> unimpl x

convArrIx :: S.ArrayIndex -> CConv Expr
convArrIx (S.ArrayIndex a is) =
  EVar <$> (LVArray <$> convExp a <*> mapM convExp is)

convNLhs :: S.Name -> CConv Expr
convNLhs n = case convName n of
  Name is -> EVar . LVName <$> convOne is

convAssign :: S.Lhs -> S.AssignOp -> S.Exp -> CConv Expr
convAssign lv op e = case lv of
  S.NameLhs   n -> convNLhs  n
  S.ArrayLhs ai -> convArrIx ai
  x             -> unimpl x

convExp :: S.Exp -> CConv Expr
convExp = \case
  S.Lit                 lit -> convLit lit
  S.ArrayCreate    t ls lex -> convArrCreate t ls lex
  S.ArrayCreateInit t dl ai -> convArrCreateI t dl ai
  S.MethodInv    mi         -> convApp mi
  S.ArrayAccess  ai         -> convArrAcc ai
  S.ExpName       n         -> convExpN n
  S.PostIncrement e         -> convStep e PostInc
  S.PostDecrement e         -> convStep e PostDec
  S.PreIncrement  e         -> convStep e PreInc
  S.PreDecrement  e         -> convStep e PreDec
  S.PrePlus       e         -> convUna e EPlus
  S.PreMinus      e         -> convUna e EMinus
  S.PreBitCompl   e         -> convUna e EBCompl
  S.PreNot        e         -> convUna e ENot
  S.Cast        t e         -> ECast <$> convTyp t <*> convExp e
  S.BinOp     l o r         -> convBOp l r o
  S.Cond      c i e         -> ECond <$> convExp c <*> convExp i <*> convExp e
  S.Assign   lv o e         -> convAssign lv o e
  x                         -> unimpl x

--------------------------------------------------------------------------------
-- Conversion, Statement:
--------------------------------------------------------------------------------

convVM :: [S.Modifier] -> CConv VarMod
convVM = \case
  []        -> pure VMNormal
  [S.Final] -> pure VMFinal
  x         -> unimpl x

convVMTyp :: [S.Modifier] -> S.Type -> CConv VMType
convVMTyp ms t = VMType <$> convVM ms <*> convTyp t

convForInit :: S.ForInit -> CConv ForInit
convForInit = u

convMay :: (x -> CConv y) -> Maybe x -> CConv (Maybe y)
convMay cv = maybe (pure Nothing) $ fmap pure . cv

getVDIId :: S.VarDeclId -> Ident
getVDIId = \case
  S.VarId i          -> convId i
  S.VarDeclArray vdi -> getVDIId vdi

convVDeclId :: S.VarDeclId -> VarDeclId
convVDeclId = u 
{-
= \case
  S.VarId i          -> VarDId $ convId i
  S.VarDeclArray vdi -> u
-}

convVDecl :: S.VarDecl -> CConv VarDecl
convVDecl (S.VarDecl vdi mvi) = do
  vdi' <- pure $ convVDeclId vdi
  mvi' <- convMay convVarInit mvi
  pure $ VarDecl vdi' mvi'

convTVVDecl :: [S.Modifier] -> S.Type -> [S.VarDecl] -> CConv TypedVVDecl
convTVVDecl mds t vds = do
  t' <- convVMTyp mds t
  vds' <- mapM convVDecl vds
  pure u

convBlock :: S.Block -> CConv Block
convBlock (S.Block bs) = Block <$> mapM convBStmt bs

convBStmt :: S.BlockStmt -> CConv Stmt
convBStmt = \case
  S.BlockStmt s         -> convStmt s
  S.LocalVars mds t vds -> SVars <$> convTVVDecl mds t vds
  x                     -> unimpl x

convStmt :: S.Stmt -> CConv Stmt
convStmt = \case
  S.Empty                   -> pure SEmpty
  S.StmtBlock b             -> SBlock  <$> convBlock b
  S.IfThen c si             -> SIf     <$> convExp c <*> convStmt si
  S.IfThenElse c si se      -> SIfElse <$> convExp c <*> convStmt si
                                                     <*> convStmt se
  S.While c si              -> SWhile  <$> convExp c <*> convStmt si
  S.Do si c                 -> SDo     <$> convExp c <*> convStmt si
  S.BasicFor mfi mc mus si  -> SForB   <$> convMay convForInit mfi
                                       <*> convMay convExp mc
                                       <*> convMay (mapM convExp) mus
                                       <*> convStmt si
  S.EnhancedFor ms t i e si -> SForE   <$> convVMTyp ms t
                                       <*> pure (convId i)
                                       <*> convExp e
                                       <*> convStmt si
  S.ExpStmt e               -> SExpr   <$> convExp e
  S.Switch e sbs            -> u
  S.Return   (Just e)       -> SReturn <$> convExp e
  S.Return   Nothing        -> pure SVReturn
  S.Break    Nothing        -> pure SBreak
  S.Continue Nothing        -> pure SContinue
  x                         -> unimpl x