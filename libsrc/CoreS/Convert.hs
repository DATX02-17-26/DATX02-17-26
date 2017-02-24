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

{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell #-}

module CoreS.Convert where

import Safe (headMay)
import Prelude hiding (LT, GT, EQ)
import Control.Monad (unless)
import Data.Maybe (isNothing)
import Data.List (sort)

import Debug.Trace.LocationTH (__LOCATION__)

import qualified Language.Java.Syntax as S

import CoreS.AST

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
  S.PrimType t -> pure $ PrimType $ convPTyp t
  S.RefType  t -> convRTyp t

convId :: S.Ident -> Ident
convId (S.Ident i) = Ident i

convName :: S.Name -> Name
convName (S.Name is) = Name $ convId <$> is

--------------------------------------------------------------------------------
-- Conversion, Expression:
--------------------------------------------------------------------------------

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
                xs  -> unimpl $__LOCATION__ xs

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
      m | m == printLn -> ESysOut <$> maybe (unimpl $__LOCATION__ args')
                                            pure (headMay args')
        | otherwise    -> pure $ EMApp m args'
  x -> unimpl $__LOCATION__ x

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
  x             -> unimpl $__LOCATION__ x

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
  x                         -> unimpl $__LOCATION__ x

--------------------------------------------------------------------------------
-- Conversion, Statement:
--------------------------------------------------------------------------------

convVM :: [S.Modifier] -> CConv VarMod
convVM = \case
  []        -> pure VMNormal
  [S.Final] -> pure VMFinal
  x         -> unimpl $__LOCATION__ x

convVMTyp :: [S.Modifier] -> S.Type -> CConv VMType
convVMTyp ms t = VMType <$> convVM ms <*> convTyp t

convForInit :: S.ForInit -> CConv ForInit
convForInit = \case
  S.ForLocalVars ms t vds -> FIVars  <$> convTVVDecl ms t vds
  S.ForInitExps  es       -> FIExprs <$> mapM convExp es

convMay :: (x -> CConv y) -> Maybe x -> CConv (Maybe y)
convMay cv = maybe (pure Nothing) $ fmap pure . cv

getVDICnt :: S.VarDeclId -> Integer
getVDICnt = \case
  S.VarId _          -> 0
  S.VarDeclArray vdi -> 1 + getVDICnt vdi

getVDIId :: S.VarDeclId -> Ident
getVDIId = \case
  S.VarId i          -> convId i
  S.VarDeclArray vdi -> getVDIId vdi

convVDeclId :: S.VarDeclId -> VarDeclId
convVDeclId vdi = case getVDICnt vdi of
  0 -> VarDId $ getVDIId vdi
  n -> VarDArr (getVDIId vdi) (getVDICnt vdi)

convVDecl :: S.VarDecl -> CConv VarDecl
convVDecl (S.VarDecl vdi mvi) =
  VarDecl <$> pure (convVDeclId vdi) <*> convMay convVarInit mvi

convTVVDecl :: [S.Modifier] -> S.Type -> [S.VarDecl] -> CConv TypedVVDecl
convTVVDecl mds t vds = TypedVVDecl <$> convVMTyp mds t <*> mapM convVDecl vds

convBlock :: S.Block -> CConv Block
convBlock (S.Block bs) = Block <$> mapM convBStmt bs

convBStmt :: S.BlockStmt -> CConv Stmt
convBStmt = \case
  S.BlockStmt s         -> convStmt s
  S.LocalVars mds t vds -> SVars <$> convTVVDecl mds t vds
  x                     -> unimpl $__LOCATION__ x

convSwitchL :: S.SwitchLabel -> CConv SwitchLabel
convSwitchL = \case
  S.SwitchCase e -> SwitchCase <$> convExp e
  S.Default      -> pure Default

convSwitchB :: S.SwitchBlock -> CConv SwitchBlock
convSwitchB (S.SwitchBlock sl bs) =
  SwitchBlock <$> convSwitchL sl <*> (Block <$> mapM convBStmt bs)

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
  S.Switch e sbs            -> SSwitch <$> convExp e <*> mapM convSwitchB sbs
  S.Return   (Just e)       -> SReturn <$> convExp e
  S.Return   Nothing        -> pure SVReturn
  S.Break    Nothing        -> pure SBreak
  S.Continue Nothing        -> pure SContinue
  x                         -> unimpl $__LOCATION__ x

--------------------------------------------------------------------------------
-- Conversion, Comp unit:
--------------------------------------------------------------------------------

convArg :: S.FormalParam -> CConv FormalParam
convArg = \case
  S.FormalParam ms t False vdi -> do
    t'   <- convVMTyp ms t
    pure $ FormalParam t' (convVDeclId vdi)
  x -> unimpl $__LOCATION__ x

convMemDecl :: S.MemberDecl -> CConv MemberDecl
convMemDecl = \case
  S.MethodDecl mds tps mrt i args exceptt me mb -> do
    ensure $__LOCATION__ mds     $ mds == [S.Public, S.Static]
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
        pure $ MethodDecl mrt' i' args' b'
  x -> unimpl $__LOCATION__ x

convDecl :: S.Decl -> CConv Decl
convDecl = \case
  S.MemberDecl md -> MemberDecl <$> convMemDecl md
  x               -> unimpl $__LOCATION__ x

convCBody :: S.ClassBody -> CConv ClassBody
convCBody (S.ClassBody ds) = ClassBody <$> mapM convDecl ds

convCDecl :: S.ClassDecl -> CConv ClassDecl
convCDecl = \case
  S.ClassDecl ms i tps ext impls body -> do
    ensure $__LOCATION__ ms    $ null ms
    ensure $__LOCATION__ tps   $ null tps
    ensure $__LOCATION__ ext   $ isNothing ext
    ensure $__LOCATION__ impls $ null impls
    ClassDecl (convId i) <$> convCBody body

convTypeDecl :: S.TypeDecl -> CConv TypeDecl
convTypeDecl = \case
  S.ClassTypeDecl cd -> ClassTypeDecl <$> convCDecl cd
  x                  -> unimpl $__LOCATION__ x

convUnit :: S.CompilationUnit -> CConv CompilationUnit
convUnit (S.CompilationUnit mpd is tds) = do
  ensure $__LOCATION__ mpd $ isNothing mpd
  ensure $__LOCATION__ is  $ null is
  CompilationUnit <$> mapM convTypeDecl tds