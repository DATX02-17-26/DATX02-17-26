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

{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell, TypeFamilies #-}

module Core.Start.Convert where

import Safe (headMay)
import Prelude hiding (LT, GT, EQ)
import Control.Monad (unless)
import Data.Maybe (isNothing)
import Data.List (nub)

import Debug.Trace.LocationTH (__LOCATION__)
import qualified Language.Java.Syntax as S

import Core.Common.History
import Core.Common.AST
import Core.Start.Phase as X
import Core.Start.AST   as X

import Util.List (isPermEq)

--------------------------------------------------------------------------------
-- Conversion:
--------------------------------------------------------------------------------

type ConvErr = String
type CConv a = Either ConvErr a

unimpl :: Show x => String -> x -> CConv y
unimpl prefix x = Left $ unwords [prefix, show x, "is not supported yet!"]

ensure :: Show x => String -> x -> Bool -> CConv ()
ensure loc x cond = unless cond $ unimpl loc x

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

strTPrefix, strT :: S.ClassType
strTPrefix = S.ClassType $ ((, []) . S.Ident) <$> ["java", "lang", "String"]
strT       = S.ClassType $ ((, []) . S.Ident) <$> ["String"]

convRTyp :: S.RefType -> CConv Type
convRTyp = \case
  S.ClassRefType ct |
    ct `elem` [strT, strTPrefix] -> pure StringT
  S.ArrayType    t               -> ArrayT <$> convTyp t
  x                              -> unimpl $__LOCATION__ x

convTyp :: S.Type -> CConv Type
convTyp = \case
  S.PrimType t -> pure $ PrimT $ convPTyp t
  S.RefType  t -> convRTyp t

revo :: (Revisable t, Label t ~ String) => Orig t -> t -> t
revo = reviseOrig "language-java.to.start"

ct :: (Revisor l d o -> () -> t) -> t
ct ctor = ctor NoH ()

convId :: S.Ident -> SIdent
convId o@(S.Ident i) = revo o $ Ident NoH i

convName :: S.Name -> SName
convName o@(S.Name is) = revo o $ Name NoH $ convId <$> is

--------------------------------------------------------------------------------
-- Conversion, Expression:
--------------------------------------------------------------------------------

convLit :: S.Literal -> CConv SExpr
convLit = pure . ELit NoH () . \case
  S.Int     i -> Int     i
  S.Word    w -> Word    w
  S.Float   f -> Float   f
  S.Double  d -> Double  d
  S.Boolean b -> Boolean b
  S.Char    c -> Char    c
  S.String  s -> String  s
  S.Null      -> Null

convUna :: S.Exp -> (Revisor String d o -> () -> SExpr -> y) -> CConv y
convUna e ctor = ct ctor <$> convExp e

convBin :: S.Exp -> S.Exp -> (SExpr -> SExpr -> y) -> CConv y
convBin l r ctor = ctor <$> convExp l <*> convExp r

convNum :: S.Exp -> S.Exp -> NumOp -> CConv SExpr
convNum l r op = convBin l r $ ct ENum op

convCmp :: S.Exp -> S.Exp -> CmpOp -> CConv SExpr
convCmp l r op = convBin l r $ ct ECmp op

convLog :: S.Exp -> S.Exp -> LogOp -> CConv SExpr
convLog l r op = convBin l r $ ct ELog op

convBOp :: S.Exp -> S.Exp -> S.Op -> CConv SExpr
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

convStep :: S.Exp -> StepOp -> CConv SExpr
convStep e op = ct EStep op <$> convExp e

convOne :: Show a => [a] -> CConv a
convOne = \case
  [x] -> pure x
  xs  -> unimpl $__LOCATION__ xs

convExpN :: S.Name -> CConv SExpr
convExpN n@(S.Name ns) = ct EVar . ct LVName . convId <$> convOne ns

convArrAcc :: S.ArrayIndex -> CConv SExpr
convArrAcc o@(S.ArrayIndex e eis) =
  ct EVar <$> (ct LVArray <$> convExp e <*> mapM convExp eis)

convArrCreate :: S.Type -> [S.Exp] -> Int -> CConv SExpr
convArrCreate t ls lex =  ct EArrNew
                      <$> convTyp t
                      <*> mapM convExp ls
                      <*> pure (toInteger lex)

convVarInit :: S.VarInit -> CConv SVarInit
convVarInit o = revo o <$> case o of
  S.InitExp    e -> InitExpr NoH <$> convExp e
  S.InitArray ai -> InitArr  NoH <$> convArrInit ai

convArrInit :: S.ArrayInit -> CConv SArrayInit
convArrInit o@(S.ArrayInit ai) = revo o . ArrayInit NoH <$> mapM convVarInit ai

convArrCreateI :: S.Type -> Int -> S.ArrayInit -> CConv SExpr
convArrCreateI t dl ai =  ct EArrNewI
                      <$> convTyp t
                      <*> pure (toInteger dl)
                      <*> convArrInit ai

printLn :: S.Name
printLn = S.Name $ S.Ident <$> ["System", "out", "println"]

convApp :: S.MethodInvocation -> CConv SExpr
convApp o = case o of
  S.MethodCall n args -> do
    args' <- mapM convExp args
    case n of
      m | m == printLn -> ct ESysOut <$> maybe (unimpl $__LOCATION__ args)
                                                   pure (headMay args')
        | otherwise    -> pure $ ct EMApp (convName m) args'
  x -> unimpl $__LOCATION__ x

convArrIx :: S.ArrayIndex -> CConv SExpr
convArrIx o@(S.ArrayIndex a is) =
  ct EVar <$> (ct LVArray <$> convExp a <*> mapM convExp is)

convNLhs :: S.Name -> CConv SExpr
convNLhs (S.Name is) = ct EVar . ct LVName . convId <$> convOne is

convAssign :: S.Lhs -> S.AssignOp -> S.Exp -> CConv SExpr
convAssign lv op e = case lv of
  S.NameLhs   n -> convNLhs  n
  S.ArrayLhs ai -> convArrIx ai
  x             -> unimpl $__LOCATION__ x

convExp :: S.Exp -> CConv SExpr
convExp o = revo o <$> case o of
  S.Lit                 l  -> convLit l
  S.ArrayCreate    t ls l  -> convArrCreate t ls l
  S.ArrayCreateInit t dl i -> convArrCreateI t dl i
  S.MethodInv    mi        -> convApp mi
  S.ArrayAccess  ai        -> convArrAcc ai
  S.ExpName       n        -> convExpN n
  S.PostIncrement e        -> convStep e PostInc
  S.PostDecrement e        -> convStep e PostDec
  S.PreIncrement  e        -> convStep e PreInc
  S.PreDecrement  e        -> convStep e PreDec
  S.PrePlus       e        -> convUna e EPlus
  S.PreMinus      e        -> convUna e EMinus
  S.PreBitCompl   e        -> convUna e EBCompl
  S.PreNot        e        -> convUna e ENot
  S.Cast        t e        -> ct ECast <$> convTyp t <*> convExp e
  S.BinOp     l o r        -> convBOp l r o
  S.Cond      c i e        -> ct ECond <$> convExp c <*> convExp i <*> convExp e
  S.Assign   lv o e        -> convAssign lv o e
  x                        -> unimpl $__LOCATION__ x

--------------------------------------------------------------------------------
-- Conversion, Statement:
--------------------------------------------------------------------------------

convVM :: [S.Modifier] -> CConv VarMod
convVM = \case
  []        -> pure VMNormal
  [S.Final] -> pure VMFinal
  x         -> unimpl $__LOCATION__ x

convVMTyp :: [S.Modifier] -> S.Type -> CConv SVMType
convVMTyp ms t = revo (ms, t) <$> (VMType NoH <$> convVM ms <*> convTyp t)

convForInit :: S.ForInit -> CConv SForInit
convForInit o = revo o <$> case o of
  S.ForLocalVars ms t vds -> FIVars  NoH <$> convTVVDecl ms t vds
  S.ForInitExps  es       -> FIExprs NoH <$> mapM convExp es

convMay :: (x -> CConv y) -> Maybe x -> CConv (Maybe y)
convMay cv = maybe (pure Nothing) $ fmap pure . cv

getVDICnt :: S.VarDeclId -> Integer
getVDICnt = \case
  S.VarId _          -> 0
  S.VarDeclArray vdi -> 1 + getVDICnt vdi

getVDIId :: S.VarDeclId -> SIdent
getVDIId = \case
  S.VarId i          -> convId i
  S.VarDeclArray vdi -> getVDIId vdi

convVDeclId :: S.VarDeclId -> SVarDeclId
convVDeclId vdi = revo vdi $ case getVDICnt vdi of
  0 -> VarDId  NoH $ getVDIId vdi
  n -> VarDArr NoH (getVDIId vdi) (getVDICnt vdi)

convVDecl :: S.VarDecl -> CConv SVarDecl
convVDecl o@(S.VarDecl vdi mvi) = revo o <$>
  (VarDecl NoH <$> pure (convVDeclId vdi) <*> convMay convVarInit mvi)

convTVVDecl :: [S.Modifier] -> S.Type -> [S.VarDecl] -> CConv STypedVVDecl
convTVVDecl mds t vds = revo (mds, t, vds) <$>
  (TypedVVDecl NoH <$> convVMTyp mds t <*> mapM convVDecl vds)

convBlock :: S.Block -> CConv SBlock
convBlock o@(S.Block bs) = revo o . ct Block <$> mapM convBStmt bs

convBStmt :: S.BlockStmt -> CConv SStmt
convBStmt o = revo o <$> case o of
  S.BlockStmt s         -> convStmt s
  S.LocalVars mds t vds -> ct SVars <$> convTVVDecl mds t vds
  x                     -> unimpl $__LOCATION__ x

convSwitchL :: S.SwitchLabel -> CConv SSwitchLabel
convSwitchL o = revo o <$> case o of
  S.SwitchCase e -> SwitchCase NoH <$> convExp e
  S.Default      -> pure $ Default NoH

convSwitchB :: S.SwitchBlock -> CConv SSwitchBlock
convSwitchB o@(S.SwitchBlock sl bs) = revo o <$>
  (SwitchBlock NoH <$> convSwitchL sl <*> (ct Block <$> mapM convBStmt bs))

convStmt :: S.Stmt -> CConv SStmt
convStmt = \case
  S.Empty                   -> pure $ ct SEmpty
  S.StmtBlock b             -> ct SBlock  <$> convBlock b
  S.IfThen c si             -> ct SIf     <$> convExp c <*> convStmt si
  S.IfThenElse c si se      -> ct SIfElse <$> convExp c <*> convStmt si
                                                        <*> convStmt se
  S.While c si              -> ct SWhile  <$> convExp c <*> convStmt si
  S.Do si c                 -> ct SDo     <$> convExp c <*> convStmt si
  S.BasicFor mfi mc mus si  -> ct SForB   <$> convMay convForInit mfi
                                          <*> convMay convExp mc
                                          <*> convMay (mapM convExp) mus
                                          <*> convStmt si
  S.EnhancedFor ms t i e si -> ct SForE   <$> convVMTyp ms t
                                          <*> pure (convId i)
                                          <*> convExp e
                                          <*> convStmt si
  S.ExpStmt e               -> ct SExpr   <$> convExp e
  S.Switch e sbs            -> ct SSwitch <$> convExp e <*> mapM convSwitchB sbs
  S.Return   (Just e)       -> ct SReturn <$> convExp e
  S.Return   Nothing        -> pure $ ct SVReturn
  S.Break    Nothing        -> pure $ ct SBreak
  S.Continue Nothing        -> pure $ ct SContinue
  x                         -> unimpl $__LOCATION__ x

--------------------------------------------------------------------------------
-- Conversion, Comp unit:
--------------------------------------------------------------------------------

convArg :: S.FormalParam -> CConv SFormalParam
convArg o = revo o <$> case o of
  S.FormalParam ms t False vdi -> do
    t'   <- convVMTyp ms t
    pure $ ct FormalParam t' (convVDeclId vdi)
  x -> unimpl $__LOCATION__ x

convMemDecl :: S.MemberDecl -> CConv SMemberDecl
convMemDecl o = revo o <$> case o of
  S.MethodDecl mds tps mrt i args exceptt me mb -> do
    ensure $__LOCATION__ mds     $ isPermEq mds [S.Public, S.Static] &&
                                   nub mds == mds
    ensure $__LOCATION__ tps     $ null tps
    ensure $__LOCATION__ exceptt $ null exceptt
    ensure $__LOCATION__ me      $ isNothing me
    case mb of
      S.MethodBody Nothing  -> unimpl $__LOCATION__ mb
      S.MethodBody (Just b) -> do
        let i' = convId i
        mrt'  <- convMay convTyp mrt
        args' <- mapM convArg args
        b'    <- convBlock b
        pure $ ct MethodDecl mrt' i' args' b'
  x -> unimpl $__LOCATION__ x

convDecl :: S.Decl -> CConv SDecl
convDecl o = revo o <$> case o of
  S.MemberDecl md -> MemberDecl NoH <$> convMemDecl md
  x               -> unimpl $__LOCATION__ x

convCBody :: S.ClassBody -> CConv SClassBody
convCBody o@(S.ClassBody ds) = revo o . ClassBody NoH <$> mapM convDecl ds

convCDecl :: S.ClassDecl -> CConv SClassDecl
convCDecl o = revo o <$> case o of
  S.ClassDecl ms i tps ext impls body -> do
    ensure $__LOCATION__ ms    $ ms == [S.Public]
    ensure $__LOCATION__ tps   $ null tps
    ensure $__LOCATION__ ext   $ isNothing ext
    ensure $__LOCATION__ impls $ null impls
    ClassDecl NoH (convId i) <$> convCBody body

convTypeDecl :: S.TypeDecl -> CConv STypeDecl
convTypeDecl o = revo o <$> case o of
  S.ClassTypeDecl cd -> ClassTypeDecl NoH <$> convCDecl cd
  x                  -> unimpl $__LOCATION__ x

convUnit :: S.CompilationUnit -> CConv SCompilationUnit
convUnit o@(S.CompilationUnit mpd is tds) = revo o <$> do
  ensure $__LOCATION__ mpd $ isNothing mpd
  ensure $__LOCATION__ is  $ null is
  CompilationUnit NoH <$> mapM convTypeDecl tds