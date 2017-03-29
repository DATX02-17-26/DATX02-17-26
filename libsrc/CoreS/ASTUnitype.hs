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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, LambdaCase, TemplateHaskell #-}

-- | The unitype AST corresponding to CoreS.AST.
module CoreS.ASTUnitype (
  -- ** Types
    AST (..)
  -- ** Conversions
  , UnitypeIso
  , toUnitype
  , fromUnitype
  , inUnitype
  , inCore
  -- ** Matching
  , canMatch
  ) where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Control.DeepSeq

import Util.TH (deriveLens)

import qualified CoreS.AST as C

--------------------------------------------------------------------------------
-- Unitype AST:
--------------------------------------------------------------------------------

-- | The unitype AST, it corresponds to constructors in CoreS.AST,
-- see that module for documentation.
data AST
  -- START: Literal
  = Int {
      _litI      :: Integer
    }
  | Word {
      _litI      :: Integer
    }
  | Float {
      _litD      :: Double
    }
  | Double {
      _litD      :: Double
    }
  | Boolean {
      _litB      :: Bool
    }
  | Char {
      _litC      :: Char
    }
  | String {
      _litS      :: String
    }
  | Null
  -- START: LValue
  | LVName {
      _lvId      :: C.Name
    }
  | LVArray {
      _lvExpr    :: AST
    , _lvExprs   :: [AST]
    }
  -- START: VarInit
  | InitExpr {
      _viExpr    :: AST
    }
  | InitArr {
      _viArrInit :: [AST]
    }
  -- Start: Expr
  | ELit {
      _eLit      :: AST
  }
  | EVar  {
      _eLValue   :: AST
  }
  | ECast {
      _eDefT     :: C.Type
    , _eExpr     :: AST
    }
  | ECond {
      _eCond     :: AST
    , _eExpI     :: AST
    , _eExpE     :: AST
  }
  | EAssign {
      _eLValue   :: AST
    , _eExpr     :: AST
  }
  | EOAssign {
      _eLValue   :: AST
    , _eNumOp    :: C.NumOp
    , _eExpr     :: AST
    }
  | ENum {
      _eNumOp    :: C.NumOp
    , _eLeft     :: AST
    , _eRight    :: AST
  }
  | ECmp {
      _eCmpOp    :: C.CmpOp
    , _eLeft     :: AST
    , _eRight    :: AST
  }
  | ELog {
      _eLogOp    :: C.LogOp
    , _eLeft     :: AST
    , _eRight    :: AST
  }
  | ENot {
      _eExpr     :: AST
    }
  | EStep {
      _eStepOp   :: C.StepOp
    , _eExpr     :: AST
    }
  | EBCompl {
      _eExpr     :: AST
    }
  | EPlus {
      _eExpr     :: AST
    }
  | EMinus {
      _eExpr     :: AST
    }
  | EMApp {
      _eName     :: C.Name
    , _eExprs    :: [AST]
    }
  | EInstNew {
      _eName     :: C.Name
    , _eExprs    :: [AST]
    }
  | EArrNew {
      _eDefT     :: C.Type
    , _eExprs    :: [AST]
    , _eEDims    :: Integer
    }
  | EArrNewI {
      _eDefT     :: C.Type
    , _eDims     :: Integer
    , _eAInit    :: [AST]
    }
  | ESysOut {
      _eExpr     :: AST
    }
  -- START: Stmt
  | SEmpty
  | Block {
      _sBlock    :: [AST]
    }
  | SExpr {
      _sExpr     :: AST
    }
  | SVars {
      -- TODO: POSSIBLE BUG!!! CONTAINS Expr!!!
      _sVDecl    :: C.TypedVVDecl
    }
  | SReturn {
      _sExpr     :: AST
    }
  | SVReturn
  | SIf {
      _sExpr     :: AST
    , _sSi       :: AST
    }
  | SIfElse {
      _sExpr     :: AST
    , _sSi       :: AST
    , _sSe       :: AST
    }
  | SWhile {
      _sExpr     :: AST
    , _sSi       :: AST
    }
  | SDo {
      _sExpr     :: AST
    , _sSi       :: AST
    }
  | SForB {
      _sForInit  :: Maybe AST
    , _sForECond :: Maybe AST
    , _sForEPost :: Maybe [AST]
    , _sSi       :: AST
    }
  | SForE {
      _sForVMTy  :: C.VMType
    , _sForVar   :: C.Ident
    , _sExpr     :: AST
    , _sSi       :: AST
    }
  | SContinue
  | SBreak
  | SSwitch {
      _sExpr      :: AST
    , _sSwiBlock  :: [AST]
    }
  -- START: SwitchBlock
  | SwitchBlock {
      -- TODO: POSSIBLE BUG!!! Should probably be AST instead of C.SwitchLabel !!!
      _sbLab      :: C.SwitchLabel
    , _sbBlock    :: [AST]
    }
  -- START: SwitchLabel
  | SwitchCase {
      _slExpr     :: AST
    }
  | Default
  -- START: ForInit
  | FIVars {
      -- TODO: POSSIBLE BUG!!! CONTAINS Expr!!!
      _fiVars     :: C.TypedVVDecl
    }
  | FIExprs {
      _fiExprs    :: [AST]
    }
  -- START: MemberDecl
  | MethodDecl {
      _mdRetrT    :: C.RType
    , _mdName     :: C.Ident
    , _mdParams   :: [C.FormalParam]
    , _mdBlock    :: [AST]
    }
  -- START: CompilationUnit
  | CompilationUnit {
      _cuImports  :: [AST]
    , _cuTDecls   :: [AST]
    }
  -- START: ImportDecl
  | ImportDecl {
      _idName     :: C.Name
    , _idStatic   :: Bool
    , _idWild     :: Bool
    }
  -- START: TypeDecl
  | ClassTypeDecl {
      _tdClass    :: AST
    }
  -- START: ClassDecl
  | ClassDecl {
      _cdId       :: C.Ident
    , _cdBody     :: AST
    }
  -- START: ClassBody
  | ClassBody {
      _cbDecls    :: [AST]
    }
  -- START: Decl
  | MemberDecl {
      _declMem    :: AST
    }
  | Hole {
      _uniHole    :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

$(deriveLens [''AST])

instance NFData AST

--------------------------------------------------------------------------------
-- Conversion class:
--------------------------------------------------------------------------------

-- | Models conversions from and to the unitype 'AST'.
--
-- Laws:
-- > fromUnitype . toUnitype = id
--
-- Inverse identity does not apply since fromUnitype is partial in nature.
-- Thus, this is not a true isomorphism, but it is for our intents and purposes.
class UnitypeIso t where
  -- | Converts a term representable in the unitype AST to the unitype AST.
  toUnitype   :: t -> AST

  -- | Converts a term in the unitype AST to one representable in it.
  -- A partial function!
  fromUnitype :: AST -> t

-- | Lifts a transformation in the unitype AST into the CoreS.AST.
inUnitype :: UnitypeIso t => (AST -> AST) -> t -> t
inUnitype f = fromUnitype . f . toUnitype

-- | Lifts a transformation in the CoreS.AST into the unitype AST.
inCore :: UnitypeIso t => (t -> t) -> AST -> AST
inCore f = toUnitype . f . fromUnitype

--------------------------------------------------------------------------------
-- Conversion DSL:
--------------------------------------------------------------------------------

(<-$) :: UnitypeIso a => (AST -> b) -> a -> b
(<-$) f x = f $ toUnitype x

(<=$) :: (Functor f, UnitypeIso a) => (f AST -> b) -> f a -> b
(<=$) f x = f $ toUnitype <$> x

(<~$) :: (Functor g, Functor f, UnitypeIso a)
      => (g (f AST) -> b) -> g (f a) -> b
(<~$) f x = f $ fmap toUnitype <$> x

($->) :: UnitypeIso a => (a -> b) -> AST -> b
($->) f x = f $ fromUnitype x

($=>) :: (Functor f, UnitypeIso b) => (f b -> a) -> f AST -> a
($=>) f x = f $ fromUnitype <$> x

($~>) :: (Functor g, Functor f, UnitypeIso b)
      => (g (f b) -> a) -> g (f AST) -> a
($~>) f x = f $ fmap fromUnitype <$> x

--------------------------------------------------------------------------------
-- Concrete conversions:
--------------------------------------------------------------------------------

instance UnitypeIso C.CompilationUnit where
  toUnitype = \case
    C.CompilationUnit is tds -> CompilationUnit <=$ is <=$ tds
    C.HoleCompilationUnit i  -> Hole i

  fromUnitype = \case
    CompilationUnit is tds -> C.CompilationUnit $=> is $=> tds
    Hole i                 -> C.HoleCompilationUnit i

instance UnitypeIso C.ImportDecl where
  toUnitype = \case
    C.ImportDecl n s w -> ImportDecl n s w
    C.HoleImportDecl i -> Hole i

  fromUnitype = \case
    ImportDecl n s w -> C.ImportDecl n s w
    Hole i           -> C.HoleImportDecl i

instance UnitypeIso C.TypeDecl where
  toUnitype = \case
    C.ClassTypeDecl cls -> ClassTypeDecl <-$ cls
    C.HoleTypeDecl i    -> Hole i

  fromUnitype = \case
    ClassTypeDecl cls -> C.ClassTypeDecl $-> cls
    Hole i            -> C.HoleTypeDecl i

instance UnitypeIso C.ClassDecl where
  toUnitype = \case
    C.ClassDecl i body -> ClassDecl i <-$ body
    C.HoleClassDecl i  -> Hole i

  fromUnitype = \case
    ClassDecl i body -> C.ClassDecl i $-> body
    Hole i           -> C.HoleClassDecl i

instance UnitypeIso C.ClassBody where
  toUnitype = \case
    C.ClassBody decls -> ClassBody <=$ decls
    C.HoleClassBody i -> Hole i
  fromUnitype = \case
    ClassBody decls -> C.ClassBody $=> decls
    Hole i          -> C.HoleClassBody i

instance UnitypeIso C.Decl where
  toUnitype = \case
    C.MemberDecl m -> MemberDecl <-$ m
    C.HoleDecl i   -> Hole i

  fromUnitype = \case
    MemberDecl m -> C.MemberDecl $-> m
    Hole i       -> C.HoleDecl i

instance UnitypeIso C.MemberDecl where
  toUnitype = \case
    C.MethodDecl m i fps (C.Block bs) -> MethodDecl m i fps <=$ bs
    C.HoleMemberDecl i                -> Hole i

  fromUnitype = \case
    MethodDecl m i fps bs -> C.MethodDecl m i fps (C.Block $=> bs)
    Hole i                -> C.HoleMemberDecl i

instance UnitypeIso C.Stmt where
  toUnitype = \case
    C.SEmpty              -> SEmpty
    C.SBlock (C.Block bs) -> Block     <=$ bs
    C.SExpr e             -> SExpr     <-$ e
    C.SVars t             -> SVars t
    C.SReturn e           -> SReturn   <-$ e
    C.SVReturn            -> SVReturn
    C.SIf     e si        -> SIf       <-$ e <-$ si
    C.SIfElse e si se     -> SIfElse   <-$ e <-$ si <-$ se
    C.SWhile  e si        -> SWhile    <-$ e <-$ si
    C.SDo     e si        -> SDo       <-$ e <-$ si
    C.SForE t i e si      -> SForE t i <-$ e <-$ si
    C.SForB mfi me mes si -> SForB     <=$ mfi <=$ me <~$ mes <-$ si
    C.SContinue           -> SContinue
    C.SBreak              -> SBreak
    C.SSwitch e lst       -> SSwitch   <-$ e <=$ lst
    C.HoleStmt i          -> Hole i

  fromUnitype = \case
    SEmpty              -> C.SEmpty
    Block bs            -> C.SBlock (C.Block $=> bs)
    SExpr e             -> C.SExpr     $-> e
    SVars t             -> C.SVars t
    SReturn e           -> C.SReturn   $-> e
    SVReturn            -> C.SVReturn
    SIf     e si        -> C.SIf       $-> e $-> si
    SIfElse e si se     -> C.SIfElse   $-> e $-> si $-> se
    SWhile  e si        -> C.SWhile    $-> e $-> si
    SDo     e si        -> C.SDo       $-> e $-> si
    SForE t i e si      -> C.SForE t i $-> e $-> si
    SForB mfi me mes si -> C.SForB     $=> mfi $=> me $~> mes $-> si
    SContinue           -> C.SContinue
    SBreak              -> C.SBreak
    SSwitch e lst       -> C.SSwitch   $-> e $=> lst
    Hole i              -> C.HoleStmt i

instance UnitypeIso C.Expr where
  toUnitype = \case
    C.ELit lit          -> ELit       <-$ lit
    C.EVar lv           -> EVar       <-$ lv
    C.ECast t e         -> ECast t    <-$ e
    C.ECond ec ei ee    -> ECond      <-$ ec      <-$ ei <-$ ee
    C.EAssign lv e      -> EAssign    <-$ lv      <-$ e
    C.EOAssign lv nop e -> (EOAssign  <-$ lv) nop <-$ e
    C.ENum op el er     -> ENum op    <-$ el      <-$ er
    C.ECmp op el er     -> ECmp op    <-$ el      <-$ er
    C.ELog op el er     -> ELog op    <-$ el      <-$ er
    C.ENot e            -> ENot       <-$ e
    C.EStep sop e       -> EStep sop  <-$ e
    C.EBCompl  e        -> EBCompl    <-$ e
    C.EPlus    e        -> EPlus      <-$ e
    C.EMinus   e        -> EMinus     <-$ e
    C.EMApp    n es     -> EMApp n    <=$ es
    C.EInstNew n es     -> EInstNew n <=$ es
    C.EArrNew  t es i   -> EArrNew t  <=$ es $ i
    C.EArrNewI t i ai   -> EArrNewI t i $ convertArrInit ai
    C.ESysOut  e        -> ESysOut    <-$ e
    C.HoleExpr i        -> Hole i

  fromUnitype = \case
    ELit lit          -> C.ELit      $-> lit
    EVar lv           -> C.EVar      $-> lv
    ECast t e         -> C.ECast t   $-> e
    ECond ec ei ee    -> C.ECond     $-> ec      $-> ei $-> ee
    EAssign lv e      -> C.EAssign   $-> lv      $-> e
    EOAssign lv nop e -> (C.EOAssign $-> lv) nop $-> e
    ENum op el er     -> C.ENum op   $-> el      $-> er
    ECmp op el er     -> C.ECmp op   $-> el      $-> er
    ELog op el er     -> C.ELog op   $-> el      $-> er
    ENot e            -> C.ENot      $-> e
    EStep sop e       -> C.EStep sop $-> e
    EBCompl  e        -> C.EBCompl   $-> e
    EPlus    e        -> C.EPlus     $-> e
    EMinus   e        -> C.EMinus    $-> e
    EMApp n es        -> C.EMApp n   $=> es
    EInstNew n es     -> C.EInstNew n $=> es
    EArrNew  t es i   -> C.EArrNew t $=> es $ i
    EArrNewI t i ai   -> C.EArrNewI t i $ convertArrInitI ai
    ESysOut  e        -> C.ESysOut   $-> e
    Hole i            -> C.HoleExpr i

instance UnitypeIso C.LValue where
  toUnitype = \case
    C.LVName n     -> LVName n
    C.LVArray e es -> LVArray <-$ e <=$ es
    C.HoleLValue i -> Hole i

  fromUnitype = \case
    LVName i     -> C.LVName i
    LVArray e es -> C.LVArray $-> e $=> es
    Hole i       -> C.HoleLValue i

instance UnitypeIso C.Literal where
  toUnitype = \case
    C.Int i         -> Int i 
    C.Word i        -> Word i  
    C.Float d       -> Float d 
    C.Double d      -> Double d 
    C.Boolean b     -> Boolean b 
    C.Char c        -> Char c 
    C.String s      -> String s 
    C.Null          -> Null
    C.HoleLiteral i -> Hole i

  fromUnitype = \case
    Int i     -> C.Int i 
    Word i    -> C.Word i  
    Float d   -> C.Float d 
    Double d  -> C.Double d 
    Boolean b -> C.Boolean b 
    Char c    -> C.Char c 
    String s  -> C.String s 
    Null      -> C.Null
    Hole i    -> C.HoleLiteral i

convertArrInit :: C.ArrayInit -> [AST]
convertArrInit (C.ArrayInit xs) = (toUnitype <$>) xs

convertArrInitI :: [AST] -> C.ArrayInit
convertArrInitI = (C.ArrayInit $=>)

instance UnitypeIso C.VarInit where
  toUnitype = \case
    C.InitExpr e    -> InitExpr <-$ e
    C.InitArr e     -> InitArr  $ convertArrInit  e
    C.HoleVarInit i -> Hole i

  fromUnitype = \case
    InitExpr e    -> C.InitExpr $-> e
    InitArr e     -> C.InitArr  $ convertArrInitI e
    Hole i        -> C.HoleVarInit i

instance UnitypeIso C.ForInit where
  toUnitype = \case
    C.FIVars v      -> FIVars v
    C.FIExprs es    -> FIExprs <=$ es
    C.HoleForInit i -> Hole i

  fromUnitype = \case
    FIVars v      -> C.FIVars v
    FIExprs es    -> C.FIExprs $=> es
    Hole i        -> C.HoleForInit i

instance UnitypeIso C.SwitchBlock where
  toUnitype = \case
    C.SwitchBlock l (C.Block bs) -> SwitchBlock l <=$ bs
    C.HoleSwitchBlock i          -> Hole i

  fromUnitype = \case
    SwitchBlock l stmts -> C.SwitchBlock l (C.Block $=> stmts)
    Hole i              -> C.HoleSwitchBlock i

--------------------------------------------------------------------------------
-- Matching:
--------------------------------------------------------------------------------

matchList :: [AST] -> [AST] -> Bool
matchList as bs
  | length as == length bs = and [canMatch a b | (a, b) <- zip as bs]
  | otherwise = False

canMatchBi :: Eq a => a -> a -> AST -> t -> AST -> AST -> Bool
canMatchBi op op' l l' r r' = op == op' && canMatch l l && canMatch r r'

matchMay :: Eq a => (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
matchMay f x x' = x == x' || fromMaybe False (f <$> x <*> x')

allc :: Foldable t => (a -> b -> Bool) -> t (a, b) -> Bool
allc = all . uncurry

allCM :: [(AST, AST)] -> Bool
allCM = allc canMatch

canMatchArr :: (Eq b, Eq a) => b -> b -> a -> a -> [AST] -> [AST] -> Bool
canMatchArr t t' i i' xs xs' = t == t' && i == i' && matchList xs xs'

-- | `canMatch complete incomplete` Tells us if the complete AST can possibly
-- match the incomplete AST.
canMatch :: AST -> AST -> Bool
canMatch = curry $ \case
  (_,                   Hole _)             -> True
  (Int i,               Int j)              -> i == j
  (Word i,              Word j)             -> i == j
  (Float i,             Float j)            -> i == j
  (Double d,            Double b)           -> d == b
  (Boolean b,           Boolean a)          -> a == b
  (Char c,              Char a)             -> a == c
  (String s,            String s')          -> s == s'
  (Null,                Null)               -> True
  (LVName i,            LVName j)           -> i == j
  (LVArray a as,        LVArray b bs)       -> canMatch a b && matchList as bs
  (InitExpr e,          InitExpr e')        -> canMatch e e'
  (InitArr  as,         InitArr bs)         -> matchList as bs
  (ELit e,              ELit e')            -> canMatch e e'
  (EVar e,              EVar e')            -> canMatch e e'
  (ECast t e,           ECast t' e')        -> t == t' && canMatch e e'
  (ECond a b c,         ECond d e f)        -> allCM [(a, d), (b, e), (c, f)]
  (ENum op l r,         ENum op' l' r')     -> canMatchBi op op' l l' r r'
  (ECmp op l r,         ECmp op' l' r')     -> canMatchBi op op' l l' r r'
  (ELog op l r,         ELog op' l' r')     -> canMatchBi op op' l l' r r'
  (ENot ast,            ENot ast')          -> canMatch ast ast'
  (EStep op e,          EStep op' e')       -> op == op' && canMatch e e'
  (EBCompl e,           EBCompl e')         -> canMatch e e'
  (EPlus   e,           EPlus   e')         -> canMatch e e'
  (EMinus  e,           EMinus  e')         -> canMatch e e'
  (EMApp c es,          EMApp d es')        -> c == d && matchList es es'
  (EInstNew c es,       EInstNew d es')     -> c == d && matchList es es'
  (EAssign a b,         EAssign c d)        -> allCM [(a, c), (b, d)]
  (EOAssign l o r,      EOAssign l' o' r')  -> canMatchBi o o' l l' r r'
  (EArrNew  t xs d,     EArrNew  t' xs' d') -> canMatchArr t t' d d' xs xs'
  (EArrNewI t i xs,     EArrNewI t' i' xs') -> canMatchArr t t' i  i'  xs xs'
  (ESysOut e,           ESysOut e')         -> canMatch e e'
  (SEmpty,              SEmpty)             -> True
  (Block ast,           Block ast')         -> matchList ast ast'
  (SExpr ast,           SExpr ast')         -> canMatch ast ast' 
  (SVars t,             SVars t')           -> t == t'
  (SReturn ast,         SReturn ast')       -> canMatch ast ast' 
  (SVReturn,            SVReturn)           -> True
  (SIf a b,             SIf c d)            -> canMatch a c && canMatch c d
  (SIfElse b f s,       SIfElse b' f' s')   -> allCM [(b, b'), (f, f'), (s, s')]
  (SWhile b bd,         SWhile b' bd')      -> allCM [(b, b'), (bd, bd')]
  (SDo b bd,            SDo b' bd')         -> allCM [(b, b'), (bd, bd')]
  (SForB a b cs d,      SForB a' b' cs' d') ->
    and [ canMatch d d', matchMay canMatch  a a', matchMay canMatch b b'
        , matchMay matchList cs cs']
  (SForE t i a b,       SForE t' i' a' b')  ->
    t == t' && i == i' && allCM [(a, a'), (b, b')]
  (SContinue,           SContinue)          -> True
  (SBreak,              SBreak)             -> True
  (SSwitch e sb,        SSwitch e' sb')     -> canMatch e e' && matchList sb sb'
  (SwitchBlock l sb,    SwitchBlock l' sb') -> l == l' && matchList sb sb'
  (SwitchCase ast,      SwitchCase ast')    -> canMatch ast ast' 
  (Default,             Default)            -> True
  (FIVars i,            FIVars j)           -> i == j
  (FIExprs as,          FIExprs bs)         -> matchList as bs
  (MethodDecl t n p b,  MethodDecl t' n' p' b') ->
    t == t' && n == n' && p == p' && matchList b b'
  (CompilationUnit i t, CompilationUnit i' t') ->
    matchList i i' && matchList t t'
  (ImportDecl n s w,    ImportDecl n' s' w') -> n == n' && s == s' && w == w'
  (ClassTypeDecl cd,    ClassTypeDecl cd')  -> canMatch cd cd' 
  (ClassDecl i cb,      ClassDecl j cb')    -> i == j && canMatch cb cb' 
  (ClassBody md,        ClassBody md')      -> matchList md md' 
  (MemberDecl ast,      MemberDecl ast')    -> canMatch ast ast'
  (_,                   _)                  -> False
