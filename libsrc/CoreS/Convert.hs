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

{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell
  , FlexibleInstances, TypeFamilies, TypeFamilyDependencies #-}

-- | Conversion to CoreS.AST from Langauge.Java.Syntax (hence: S).
module CoreS.Convert (
  -- * Types
    ConvErr
  , CConv
  , Repr
  -- * Classes
  , ToCoreS
  -- * Operations
  , toCoreS
  ) where

import Safe (headMay)
import Prelude hiding (LT, GT, EQ)
import Control.Monad (unless, (>=>))
import Data.Maybe (isNothing)
import Data.List (nub)

import Debug.Trace.LocationTH (__LOCATION__)

import Util.List (isPermEq)

import qualified Language.Java.Syntax as S
import CoreS.AST

--------------------------------------------------------------------------------
-- Conversion computation type:
--------------------------------------------------------------------------------

type ConvErr = String
type CConv a = Either ConvErr a

--------------------------------------------------------------------------------
-- Conversion class:
--------------------------------------------------------------------------------

-- | Models the conversion to CoreS.AST.
class ToCoreS t where
  -- | The corresponding data type in CoreS.AST.
  type Repr t = (r :: *) | r -> t

  -- | Convert from S. The conversion is partial due to possible holes.
  toCoreS :: t -> CConv (Repr t)

--------------------------------------------------------------------------------
-- Conversion DSL:
--------------------------------------------------------------------------------

unimpl :: Show x => String -> x -> CConv y
unimpl prefix x = Left $ unwords [prefix, show x, "is not supported yet!"]

ensure :: Show x => String -> x -> Bool -> CConv ()
ensure loc x cond = unless cond $ unimpl loc x

toCoreSM :: (Traversable f, ToCoreS t) => f t -> CConv (f (Repr t))
toCoreSM = mapM toCoreS

(<-$) :: ToCoreS t => (Repr t -> b) -> t -> CConv b
(<-$) f = fmap f . toCoreS

(<-*) :: ToCoreS t => CConv (Repr t -> b) -> t -> CConv b
(<-*) f x = f <*> toCoreS x

(<=$) :: (Traversable f, ToCoreS t) => (f (Repr t) -> b) -> f t -> CConv b
(<=$) f = fmap f . toCoreSM

(<=*) :: (Traversable f, ToCoreS t) => CConv (f (Repr t) -> b) -> f t -> CConv b
(<=*) f x = f <*> toCoreSM x

(<~*) :: (Traversable g, Traversable f, ToCoreS t)
      => CConv (g (f (Repr t)) -> b) -> g (f t) -> CConv b
(<~*) f x = f <*> mapM toCoreSM x

infixl 4 <-$, <-*, <=$, <=*, <~*

--------------------------------------------------------------------------------
-- Conversion, Types:
--------------------------------------------------------------------------------

instance ToCoreS S.PrimType where
  type Repr S.PrimType = PrimType
  toCoreS = pure . \case
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
  S.ArrayType    t               -> ArrayT <-$ t
  x                              -> unimpl $__LOCATION__ x

instance ToCoreS S.Type where
  type Repr S.Type = Type
  toCoreS = \case
    S.PrimType t -> PrimT <-$ t
    S.RefType  t -> convRTyp t

instance ToCoreS S.Ident where
  type Repr S.Ident = Ident
  toCoreS = \case
    S.Ident i -> pure $ Ident i

instance ToCoreS S.Name where
  type Repr S.Name = Name
  toCoreS = \case
    S.Name is -> Name <=$ is

--------------------------------------------------------------------------------
-- Conversion, Expression:
--------------------------------------------------------------------------------

instance ToCoreS S.Literal where
  type Repr S.Literal = Literal
  toCoreS = pure . \case
    S.Int     i -> Int     i
    S.Word    w -> Word    w
    S.Float   f -> Float   f
    S.Double  d -> Double  d
    S.Boolean b -> Boolean b
    S.Char    c -> Char    c
    S.String  s -> String  s
    S.Null      -> Null

convNum :: S.Exp -> S.Exp -> NumOp -> CConv Expr
convNum l r op = ENum op <-$ l <-* r

convCmp :: S.Exp -> S.Exp -> CmpOp -> CConv Expr
convCmp l r op = ECmp op <-$ l <-* r

convLog :: S.Exp -> S.Exp -> LogOp -> CConv Expr
convLog l r op = ELog op <-$ l <-* r

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

convOne :: Show a => [a] -> CConv a
convOne = \case [x] -> pure x
                xs  -> unimpl $__LOCATION__ xs

instance ToCoreS S.VarInit where
  type Repr S.VarInit = VarInit
  toCoreS = \case
    S.InitExp    e -> InitExpr <-$ e
    S.InitArray ai -> InitArr  <-$ ai

instance ToCoreS S.ArrayInit where
  type Repr S.ArrayInit = ArrayInit
  toCoreS = \case
    S.ArrayInit ai -> ArrayInit <=$ ai

printLn :: Name
printLn = Name $ Ident <$> ["System", "out", "println"]

convApp :: S.MethodInvocation -> CConv Expr
convApp = \case
  S.MethodCall n args -> do
    args' <- toCoreSM args
    n'    <- toCoreS n
    case n' of
      m | m == printLn -> ESysOut <$> maybe (unimpl $__LOCATION__ args')
                                            pure (headMay args')
        | otherwise    -> pure $ EMApp m args'
  x -> unimpl $__LOCATION__ x

instance ToCoreS S.AssignOp where
  type Repr S.AssignOp = NumOp
  toCoreS x = case x of
    S.EqualA   -> -- represented differently, but supported.
                  unimpl $__LOCATION__ x
    S.MultA    -> pure Mul
    S.DivA     -> pure Div
    S.RemA     -> pure Rem
    S.AddA     -> pure Add
    S.SubA     -> pure Sub
    S.LShiftA  -> pure LShift
    S.RShiftA  -> pure RShift
    S.RRShiftA -> pure RRShift
    S.AndA     -> pure And
    S.XorA     -> pure Xor
    S.OrA      -> pure Or

instance ToCoreS S.Lhs where
  type Repr S.Lhs = LValue
  toCoreS = \case
    S.NameLhs   n                  -> toCoreS n >>= \(Name is) ->
                                      LVName <$> convOne is
    S.ArrayLhs (S.ArrayIndex a is) -> LVArray <-$ a <=* is
    x                              -> unimpl $__LOCATION__ x

convAssign :: S.Lhs -> S.Exp -> S.AssignOp -> CConv Expr
convAssign lv e = \case
  S.EqualA -> EAssign  <-$ lv <-* e
  op       -> EOAssign <-$ lv <-* op <-* e

instance ToCoreS S.Exp where
  type Repr S.Exp = Expr
  toCoreS = \case
    S.Lit                 lit -> ELit <-$ lit
    S.ArrayCreate    t ls lex -> EArrNew  <-$ t <=* ls <*> pure (toInteger lex)
    S.ArrayCreateInit t dl ai -> EArrNewI <-$ t <*> pure (toInteger dl) <-* ai
    S.MethodInv    mi         -> convApp mi
    S.ArrayAccess
      (S.ArrayIndex e eis)    -> EVar <$> (LVArray <-$ e <=* eis)
    S.ExpName (S.Name ns)     -> convOne ns >>= (EVar . LVName <-$)
    S.PostIncrement e         -> EStep PostInc <-$ e
    S.PostDecrement e         -> EStep PostDec <-$ e
    S.PreIncrement  e         -> EStep PreInc  <-$ e
    S.PreDecrement  e         -> EStep PreDec  <-$ e
    S.PrePlus       e         -> EPlus         <-$ e
    S.PreMinus      e         -> EMinus        <-$ e
    S.PreBitCompl   e         -> EBCompl       <-$ e
    S.PreNot        e         -> ENot          <-$ e
    S.Cast        t e         -> ECast <-$ t <-* e
    S.BinOp     l o r         -> convBOp l r o
    S.Cond      c i e         -> ECond <-$ c <-* i <-* e
    S.Assign   lv o e         -> convAssign lv e o
    x                         -> unimpl $__LOCATION__ x

--------------------------------------------------------------------------------
-- Conversion, Statement:
--------------------------------------------------------------------------------

convVM :: [S.Modifier] -> CConv VarMod
convVM = \case
  []        -> pure VMNormal
  [S.Final] -> pure VMFinal
  x         -> unimpl $__LOCATION__ x

instance ToCoreS ([S.Modifier], S.Type) where
  type Repr ([S.Modifier], S.Type) = VMType
  toCoreS = \case
    (ms, t) -> VMType <$> convVM ms <-* t

instance ToCoreS S.ForInit where
  type Repr S.ForInit = ForInit
  toCoreS = \case
    S.ForLocalVars mds t vds -> FIVars  <-$ ((mds, t), vds)
    S.ForInitExps  es        -> FIExprs <=$ es

getVDICnt :: S.VarDeclId -> CConv Integer
getVDICnt = \case
  S.VarId _          -> pure 0
  S.VarDeclArray vdi -> (1+) <$> getVDICnt vdi

getVDIId :: S.VarDeclId -> CConv Ident
getVDIId = \case
  S.VarId i          -> toCoreS i
  S.VarDeclArray vdi -> getVDIId vdi

instance ToCoreS S.VarDeclId where
  type Repr S.VarDeclId = VarDeclId
  toCoreS vdi = getVDICnt vdi >>= \case
    0 -> VarDId  <$> getVDIId vdi
    n -> VarDArr <$> getVDIId vdi <*> getVDICnt vdi

instance ToCoreS S.VarDecl where
  type Repr S.VarDecl = VarDecl
  toCoreS = \case
    S.VarDecl vdi mvi -> VarDecl <-$ vdi <=* mvi

instance ToCoreS (([S.Modifier], S.Type), [S.VarDecl]) where
  type Repr (([S.Modifier], S.Type), [S.VarDecl]) = TypedVVDecl
  toCoreS = \case
    (vmt, vds) -> TypedVVDecl <-$ vmt <=* vds

instance ToCoreS S.Block where
  type Repr S.Block = Block
  toCoreS = \case
    S.Block bs -> Block <=$ bs

instance ToCoreS S.BlockStmt where
  type Repr S.BlockStmt = Stmt
  toCoreS = \case
    S.BlockStmt s         -> convStmt s
    S.LocalVars mds t vds -> SVars <-$ ((mds, t), vds)
    x                     -> unimpl $__LOCATION__ x

instance ToCoreS S.SwitchLabel where
  type Repr S.SwitchLabel = SwitchLabel
  toCoreS = \case
    S.SwitchCase e -> SwitchCase <-$ e
    S.Default      -> pure Default

instance ToCoreS S.SwitchBlock where
  type Repr S.SwitchBlock = SwitchBlock
  toCoreS = \case
    S.SwitchBlock sl bs -> SwitchBlock <-$ sl <*> (Block <=$ bs)

-- Can't be an instance due to injectivity:
convStmt :: S.Stmt -> CConv Stmt
convStmt = \case
    S.Empty                   -> pure SEmpty
    S.StmtBlock b             -> SBlock  <-$ b
    S.IfThen c si             -> SIf     <-$ c <*> convStmt si
    S.IfThenElse c si se      -> SIfElse <-$ c <*> convStmt si <*> convStmt se
    S.While c si              -> SWhile  <-$ c <*> convStmt si
    S.Do si c                 -> SDo     <-$ c <*> convStmt si
    S.BasicFor mfi mc mus si  -> SForB   <=$ mfi <=* mc <~* mus
                                               <*> convStmt si
    S.EnhancedFor ms t i e si -> SForE   <-$ (ms, t) <-* i <-* e
                                               <*> convStmt si
    S.ExpStmt e               -> SExpr   <-$ e
    S.Switch e sbs            -> SSwitch <-$ e <=* sbs
    S.Return   (Just e)       -> SReturn <-$ e
    S.Return   Nothing        -> pure SVReturn
    S.Break    Nothing        -> pure SBreak
    S.Continue Nothing        -> pure SContinue
    x                         -> unimpl $__LOCATION__ x

--------------------------------------------------------------------------------
-- Conversion, Comp unit:
--------------------------------------------------------------------------------

instance ToCoreS S.FormalParam where
  type Repr S.FormalParam = FormalParam
  toCoreS = \case
    S.FormalParam ms t False vdi -> FormalParam <-$ (ms, t) <-* vdi
    x -> unimpl $__LOCATION__ x

instance ToCoreS S.MemberDecl where
  type Repr S.MemberDecl = MemberDecl
  toCoreS = \case
    S.MethodDecl mds tps mrt i args exceptt me mb -> do
      ensure $__LOCATION__ mds     $ isPermEq mds [S.Public, S.Static] &&
                                     nub mds == mds
      ensure $__LOCATION__ tps     $ null tps
      ensure $__LOCATION__ exceptt $ null exceptt
      ensure $__LOCATION__ me      $ isNothing me
      case mb of
        S.MethodBody Nothing  -> unimpl $__LOCATION__ mb
        S.MethodBody (Just b) -> MethodDecl <=$ mrt <-* i <=* args <-* b
    x -> unimpl $__LOCATION__ x

instance ToCoreS S.Decl where
  type Repr S.Decl = Decl
  toCoreS = \case
    S.MemberDecl md -> MemberDecl <-$ md
    x               -> unimpl $__LOCATION__ x

instance ToCoreS S.ClassBody where
  type Repr S.ClassBody = ClassBody
  toCoreS = \case
    S.ClassBody ds -> ClassBody <=$ ds

instance ToCoreS S.ClassDecl where
  type Repr S.ClassDecl = ClassDecl
  toCoreS = \case
    S.ClassDecl ms i tps ext impls body -> do
      ensure $__LOCATION__ ms    $ ms == [S.Public]
      ensure $__LOCATION__ tps   $ null tps
      ensure $__LOCATION__ ext   $ isNothing ext
      ensure $__LOCATION__ impls $ null impls
      ClassDecl <-$ i <-* body

instance ToCoreS S.TypeDecl where
  type Repr S.TypeDecl = TypeDecl
  toCoreS = \case
    S.ClassTypeDecl cd -> ClassTypeDecl <-$ cd
    x                  -> unimpl $__LOCATION__ x

instance ToCoreS S.CompilationUnit where
  type Repr S.CompilationUnit = CompilationUnit
  toCoreS = \case
    S.CompilationUnit mpd is tds -> do
      ensure $__LOCATION__ mpd $ isNothing mpd
      ensure $__LOCATION__ is  $ null is
      CompilationUnit <=$ tds