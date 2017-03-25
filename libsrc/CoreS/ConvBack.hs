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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, LambdaCase, TemplateHaskell
  , TypeFamilies, FlexibleContexts, ConstraintKinds #-}

-- | Conversion back to Langauge.Java.Syntax (hence: S).
module CoreS.ConvBack (
  -- * Types
    HoleSum (..)
  , LJSynConv
  , Repr
  -- * Classes
  , ToLJSyn
  , ToLJSynP
  -- * Operations
  , toLJSyn
  , prettyCore
  , prettyCore'
  , dumpCore
  ) where

import Prelude hiding (EQ, LT, GT)

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Bifunctor (first)
import Data.Function.Pointless ((.:))
import Control.Monad ((>=>))
import Control.Lens ((^?))

import Util.TH (deriveLens)
import Util.Debug (exitLeft)

import qualified Language.Java.Pretty as P
import qualified Language.Java.Syntax as S

import CoreS.AST

-- TODO: import Util.Function (in feature/norm/vardecl)

-- | 'applyN': applies a function f, i times to x. Therefore applyN 0 id == id.
applyN :: (Eq i, Num i) => i -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN i f x = applyN (i - 1) f $ f x

--------------------------------------------------------------------------------
-- Errors:
--------------------------------------------------------------------------------

-- | Sum of types in CoreS.AST containing holes or might otherwise fail here.
data HoleSum
  = HSType {
      _hsType :: Type
    }
  | HSLiteral {
      _hsLiteral :: Literal
    }
  | HSLValue {
      _hLValueX :: LValue
    }
  | HSVarInit {
      _hsVarInit :: VarInit
    }
  | HSExpr {
      _hsExpr :: Expr
    }
  | HSVarDeclId {
      _hsVarDeclId :: VarDeclId
    }
  | HSVarDecl {
      _hsVarDecl :: VarDecl
    }
  | HSVMType {
      _hsVMType :: VMType
    }
  | HSTypedVVDecl {
      _hsTypedVVDecl :: TypedVVDecl
    }
  | HSForInit {
      _hsForInit :: ForInit
    }
  | HSSwitchLabel {
      _hsSwitchLabel :: SwitchLabel
    }
  | HSSwitchBlock {
      _hsSwitchBlock :: SwitchBlock
    }
  | HSBlock {
      _hsBlock :: Block
    }
  | HSStmt {
      _hsStmt :: Stmt
    }
  | HSMemberDecl {
      _hsMemberDecl :: MemberDecl
    }
  | HSDecl {
      _hsDecl :: Decl
    }
  | HSClassBody {
      _hsClassBody :: ClassBody
    }
  | HSClassDecl {
      _hsClassDecl :: ClassDecl
    }
  | HSTypeDecl {
      _hsTypeDecl ::  TypeDecl
    }
  | HSImportDecl {
      _hsImportDecl :: ImportDecl
    }
  | HSCompilationUnit {
      _hsCompilationUnit :: CompilationUnit
    }
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveLens [''HoleSum])

--------------------------------------------------------------------------------
-- Conversion computation type:
--------------------------------------------------------------------------------

type LJSynConv a = Either HoleSum a

--------------------------------------------------------------------------------
-- Converting back class:
--------------------------------------------------------------------------------

-- | Models the conversion back to S.
class ToLJSyn t where
  -- | The corresponding data type in S.
  type Repr t :: *

  -- | Convert back to S. The conversion is partial due to
  -- possible holes.
  toLJSyn :: t -> LJSynConv (Repr t)

-- | Combined constraint for things convertible back to S,
-- and which in turn in S is convertible to String via prettyfication.
type ToLJSynP ast = (ToLJSyn ast, P.Pretty (Repr ast))

-- | Prettified a term in CoreS.AST as the Java syntax tree representation.
prettyCore :: ToLJSynP ast => ast -> LJSynConv String
prettyCore core = P.prettyPrint <$> toLJSyn core

-- | Prettified a term in CoreS.AST as the Java syntax tree representation.
-- On error, shows the error.
prettyCore' :: ToLJSynP ast => ast -> Either String String
prettyCore' core = first show $ prettyCore core

-- | Dumps a CoreS.AST term prettified as the syntax tree in Java.
dumpCore :: ToLJSynP ast => ast -> IO ()
dumpCore = exitLeft . prettyCore >=> putStrLn

--------------------------------------------------------------------------------
-- Conversion DSL:
--------------------------------------------------------------------------------

toLJSynM :: (Traversable f, ToLJSyn t) => f t -> LJSynConv (f (Repr t))
toLJSynM = mapM toLJSyn

(<-$) :: ToLJSyn t => (Repr t -> b) -> t -> LJSynConv b
(<-$) f = fmap f . toLJSyn

(<-*) :: ToLJSyn t => LJSynConv (Repr t -> b) -> t -> LJSynConv b
(<-*) f x = f <*> toLJSyn x

(<=$) :: (Traversable f, ToLJSyn t) => (f (Repr t) -> b) -> f t -> LJSynConv b
(<=$) f = fmap f . toLJSynM

(<=*) :: (Traversable f, ToLJSyn t)
      => LJSynConv (f (Repr t) -> b) -> f t -> LJSynConv b
(<=*) f x = f <*> toLJSynM x

(<~$) :: (Traversable g, Traversable f, ToLJSyn t)
      => (g (f (Repr t)) -> b) -> g (f t) -> LJSynConv b
(<~$) f = fmap f . mapM toLJSynM

(<~*) :: (Traversable g, Traversable f, ToLJSyn t)
      => LJSynConv (g (f (Repr t)) -> b) -> g (f t) -> LJSynConv b
(<~*) f x = f <*> mapM toLJSynM x

infixl 4 <-$, <-*, <=$, <=*, <~$, <~*

--------------------------------------------------------------------------------
-- Concrete conversions, Names and identifiers:
--------------------------------------------------------------------------------

instance ToLJSyn Ident where
  type Repr Ident = S.Ident
  toLJSyn = \case
    Ident i -> pure $ S.Ident i

instance ToLJSyn Name where
  type Repr Name = S.Name
  toLJSyn = \case
    Name is -> S.Name <=$ is

--------------------------------------------------------------------------------
-- Concrete conversions, Types:
--------------------------------------------------------------------------------

instance ToLJSyn PrimType where
  type Repr PrimType = S.PrimType
  toLJSyn = pure . \case
    BoolT   -> S.BooleanT
    ByteT   -> S.ByteT
    ShortT  -> S.ShortT
    IntT    -> S.IntT
    LongT   -> S.LongT
    CharT   -> S.CharT
    FloatT  -> S.FloatT
    DoubleT -> S.DoubleT

instance ToLJSyn Type where
  type Repr Type = S.Type
  toLJSyn = \case
    PrimT  t -> S.PrimType <-$ t
    ArrayT t -> S.RefType . S.ArrayType  <-$ t
    StringT  -> pure $ S.RefType $ S.ClassRefType $
                       S.ClassType [(S.Ident "String", [])]
    NullT    -> Left $ HSType NullT

--------------------------------------------------------------------------------
-- Concrete conversions, Literals:
--------------------------------------------------------------------------------

instance ToLJSyn Literal where
  type Repr Literal = S.Literal
  toLJSyn = pure . \case
    Int     i -> S.Int     i
    Word    w -> S.Word    w
    Float   f -> S.Float   f
    Double  d -> S.Double  d
    Boolean b -> S.Boolean b
    Char    c -> S.Char    c
    String  s -> S.String  s
    Null      -> S.Null

--------------------------------------------------------------------------------
-- Concrete conversions, lvalues:
--------------------------------------------------------------------------------

instance ToLJSyn LValue where
  type Repr LValue = S.Lhs
  toLJSyn x = case x of
    LVName i     -> S.NameLhs . S.Name . pure <-$ i
    LVArray e es -> S.ArrayLhs .: S.ArrayIndex <-$ e <=* es
    HoleLValue i -> Left $ HSLValue x

--------------------------------------------------------------------------------
-- Concrete conversions, Variable & Array initialization:
--------------------------------------------------------------------------------

instance ToLJSyn ArrayInit where
  type Repr ArrayInit = S.ArrayInit
  toLJSyn = \case
    ArrayInit xs -> S.ArrayInit <=$ xs

instance ToLJSyn VarInit where
  type Repr VarInit = S.VarInit
  toLJSyn x = case x of
    InitExpr e    -> S.InitExp   <-$ e
    InitArr ai    -> S.InitArray <-$ ai
    HoleVarInit i -> Left $ HSVarInit x

--------------------------------------------------------------------------------
-- Concrete conversions, Expressions:
--------------------------------------------------------------------------------

stepOp :: StepOp -> S.Exp -> S.Exp
stepOp = \case
  PostInc -> S.PostIncrement
  PostDec -> S.PostDecrement
  PreInc  -> S.PreIncrement
  PreDec  -> S.PreDecrement

appOp :: (t -> S.Op) -> t -> S.Exp -> S.Exp -> S.Exp
appOp f t l = S.BinOp l (f t)

logOp :: LogOp -> S.Op
logOp = \case
  LAnd -> S.CAnd
  LOr  -> S.COr

numOp :: NumOp -> S.Op
numOp = \case
  Add     -> S.Add
  Sub     -> S.Sub
  Mul     -> S.Mult
  Div     -> S.Div
  Rem     -> S.Rem
  LShift  -> S.LShift
  RShift  -> S.RShift
  RRShift -> S.RRShift
  And     -> S.And
  Xor     -> S.Xor
  Or      -> S.Or

numOpA :: NumOp -> S.AssignOp
numOpA = \case
  Add     -> S.AddA
  Sub     -> S.SubA
  Mul     -> S.MultA
  Div     -> S.DivA
  Rem     -> S.RemA
  LShift  -> S.LShiftA
  RShift  -> S.RShiftA
  RRShift -> S.RRShiftA
  And     -> S.AndA
  Xor     -> S.XorA
  Or      -> S.OrA

cmpOp :: CmpOp -> S.Op
cmpOp = \case
  EQ -> S.Equal
  NE -> S.NotEq
  LT -> S.LThan
  GT -> S.GThan
  LE -> S.LThanE
  GE -> S.GThanE

println :: Name
println = Name (Ident <$> ["System", "out", "println"])

mkInt :: Applicative f => Integer -> f Int
mkInt = pure . fromInteger

instance ToLJSyn Expr where
  type Repr Expr = S.Exp
  toLJSyn x = case x of
    ELit l                   -> S.Lit <-$ l
    EVar lv                  -> toLJSyn lv >>= \case
      S.NameLhs  n  -> pure $ S.ExpName n
      S.ArrayLhs ai -> pure $ S.ArrayAccess ai
      _             -> Left $ HSLValue lv
    ECast t e                -> S.Cast <-$ t <-* e
    ECond c ei ee            -> S.Cond <-$ c <-* ei <-* ee
    EAssign lv e             -> S.Assign <-$ lv <*> pure S.EqualA <-* e
    EOAssign lv op e         -> S.Assign <-$ lv <*> pure (numOpA op) <-* e
    ENum op l r              -> appOp numOp op <-$ l <-* r
    ECmp op l r              -> appOp cmpOp op <-$ l <-* r
    ELog op l r              -> appOp logOp op <-$ l <-* r
    EStep op e               -> stepOp op      <-$ e
    ENot e                   -> S.PreNot       <-$ e
    EBCompl  e               -> S.PreBitCompl  <-$ e
    EPlus    e               -> S.PrePlus      <-$ e
    EMinus   e               -> S.PreMinus     <-$ e
    EMApp n es               -> S.MethodInv <$> (S.MethodCall <-$ n <=* es)
    EArrNew  t es i          -> S.ArrayCreate     <-$ t <=* es <*> mkInt i
    EArrNewI t i ai          -> S.ArrayCreateInit <-$ t <*> mkInt i <-* ai
    ESysOut  e               -> toLJSyn $ EMApp println [e]
    HoleExpr i               -> Left $ HSExpr x

--------------------------------------------------------------------------------
-- Concrete conversions, Statements:
--------------------------------------------------------------------------------

instance ToLJSyn VarDeclId where
  type Repr VarDeclId = S.VarDeclId
  toLJSyn x = case x of
    VarDId i        -> S.VarId <-$ i
    VarDArr i dim   -> applyN dim S.VarDeclArray . S.VarId <-$ i
    HoleVarDeclId i -> Left $ HSVarDeclId x

instance ToLJSyn VarDecl where
  type Repr VarDecl = S.VarDecl
  toLJSyn x = case x of
    VarDecl vdi mvi -> S.VarDecl <-$ vdi <=* mvi
    HoleVarDecl i   -> Left $ HSVarDecl x

instance ToLJSyn VarMod where
  type Repr VarMod = [S.Modifier]
  toLJSyn = pure . \case
    VMFinal  -> [S.Final]
    VMNormal -> []

instance ToLJSyn VMType where
  type Repr VMType = ([S.Modifier], S.Type)
  toLJSyn x = case x of
    VMType mod t -> (,) <-$ mod <-* t
    HoleVMType i -> Left $ HSVMType x

instance ToLJSyn TypedVVDecl where
  type Repr TypedVVDecl = (([S.Modifier], S.Type), [S.VarDecl])
  toLJSyn x = case x of
    TypedVVDecl vmt vds -> (,) <-$ vmt <=* vds
    HoleTypedVVDecl i   -> Left $ HSTypedVVDecl x

instance ToLJSyn ForInit where
  type Repr ForInit = S.ForInit
  toLJSyn x = case x of
    FIVars tvd    -> uncurry (uncurry S.ForLocalVars) <-$ tvd
    FIExprs es    -> S.ForInitExps <=$ es
    HoleForInit i -> Left $ HSForInit x

instance ToLJSyn SwitchLabel where
  type Repr SwitchLabel = S.SwitchLabel
  toLJSyn x = case x of
    SwitchCase e      -> S.SwitchCase <-$ e
    Default           -> pure S.Default
    HoleSwitchLabel i -> Left $ HSSwitchLabel x

instance ToLJSyn SwitchBlock where
  type Repr SwitchBlock = S.SwitchBlock
  toLJSyn x = case x of
    SwitchBlock l blk -> do
       ss   <- maybe (Left $ HSBlock blk) pure (blk ^? bStmts)
       S.SwitchBlock <-$ l <=* ss
    HoleSwitchBlock i -> Left $ HSSwitchBlock x

instance ToLJSyn Block where
  type Repr Block = S.Block
  toLJSyn x = case x of
    Block ss    -> S.Block <=$ ss
    HoleBlock i -> Left $ HSBlock x

mkStmt :: Applicative f => S.Stmt -> f S.BlockStmt
mkStmt = pure . S.BlockStmt

mkSSimp :: Applicative f => (Maybe a -> S.Stmt) -> f S.BlockStmt
mkSSimp = mkStmt . ($ Nothing)

unBS :: S.BlockStmt -> S.Stmt
unBS = \case
  S.BlockStmt s -> s
  _             -> error "unBS only supports extracting S.Stmt"

instance ToLJSyn Stmt where
  type Repr Stmt = S.BlockStmt
  toLJSyn x = case x of
    SEmpty           -> mkStmt S.Empty
    SVReturn         -> mkSSimp S.Return
    SReturn e        -> S.Return . pure <-$ e >>= mkStmt
    SBlock b         -> S.StmtBlock     <-$ b >>= mkStmt
    SExpr e          -> S.ExpStmt       <-$ e >>= mkStmt
    SVars tvd        -> uncurry (uncurry S.LocalVars) <-$ tvd
    SIf     e si     -> S.IfThen <-$ e <*> (unBS <-$ si) >>= mkStmt
    SIfElse e si se  -> S.IfThenElse <-$ e <*> (unBS <-$ si) <*> (unBS <-$ se)
                    >>= mkStmt
    SWhile  e si     -> S.While  <-$ e <*> (unBS <-$ si) >>= mkStmt
    SDo     e si     -> S.Do     <$> (unBS <-$ si) <-* e >>= mkStmt
    SForB fi e es si -> S.BasicFor <=$ fi <=* e <~* es <*> (unBS <-$ si)
                    >>= mkStmt
    SForE t i e si  -> uncurry S.EnhancedFor <-$ t <-* i <-* e <*> (unBS <-$ si)
                    >>= mkStmt
    SContinue        -> mkSSimp S.Continue
    SBreak           -> mkSSimp S.Break
    SSwitch e sbs    -> S.Switch <-$ e <=* sbs >>= mkStmt
    HoleStmt i       -> Left $ HSStmt x

--------------------------------------------------------------------------------
-- Concrete conversions, Method:
--------------------------------------------------------------------------------

instance ToLJSyn FormalParam where
  type Repr FormalParam = S.FormalParam
  toLJSyn = \case
    FormalParam vmt vdi -> uncurry S.FormalParam <-$ vmt <*> pure False <-* vdi

instance ToLJSyn MemberDecl where
  type Repr MemberDecl = S.MemberDecl
  toLJSyn x = case x of
    MethodDecl rt i fps blk -> do
      rt'  <- toLJSynM rt
      i'   <- toLJSyn  i
      fps' <- toLJSynM fps
      body <- (S.MethodBody . pure) <-$ blk
      pure $ S.MethodDecl [S.Public, S.Static] [] rt' i' fps' [] Nothing body
    HoleMemberDecl i -> Left $ HSMemberDecl x

--------------------------------------------------------------------------------
-- Concrete conversions, Compilation Unit:
--------------------------------------------------------------------------------

instance ToLJSyn Decl where
  type Repr Decl = S.Decl
  toLJSyn x = case x of
    MemberDecl m -> S.MemberDecl <-$ m
    HoleDecl i   -> Left $ HSDecl x

instance ToLJSyn ClassBody where
  type Repr ClassBody = S.ClassBody
  toLJSyn x = case x of
    ClassBody decls -> S.ClassBody <=$ decls
    HoleClassBody i -> Left $ HSClassBody x

instance ToLJSyn ClassDecl where
  type Repr ClassDecl = S.ClassDecl
  toLJSyn x = case x of
    ClassDecl i b -> do
      i' <- toLJSyn i
      b' <- toLJSyn b
      pure $ S.ClassDecl [S.Public] i' [] Nothing [] b'
    HoleClassDecl i  -> Left $ HSClassDecl x

instance ToLJSyn TypeDecl where
  type Repr TypeDecl = S.TypeDecl
  toLJSyn x = case x of
    ClassTypeDecl cd -> S.ClassTypeDecl <-$ cd
    HoleTypeDecl i   -> Left $ HSTypeDecl x

instance ToLJSyn ImportDecl where
  type Repr ImportDecl = S.ImportDecl
  toLJSyn x = case x of
    ImportDecl n s w -> S.ImportDecl s <-$ n <*> pure w
    HoleImportDecl i -> Left $ HSImportDecl x

instance ToLJSyn CompilationUnit where
  type Repr CompilationUnit = S.CompilationUnit
  toLJSyn x = case x of
    CompilationUnit is tds -> S.CompilationUnit Nothing <=$ is <=* tds
    HoleCompilationUnit i  -> Left $ HSCompilationUnit x