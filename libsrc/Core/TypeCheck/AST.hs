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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE GADTs, DataKinds, PolyKinds, KindSignatures, TypeFamilies
  , LambdaCase, TypeOperators, TemplateHaskell, TypeSynonymInstances
  , FlexibleInstances #-}

module Core.TypeCheck.AST where

import Data.Proxy (Proxy (..))
import Data.Void (Void)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import Core.Common.TH
import Core.Common.History
import Core.Common.AST
import Core.TypeCheck.Phase as X
import Core.Start.AST

import Control.Lens ((%~))

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

-- | Singleton versions of PrimType.
data SPrimType (t :: PrimType) where
  TBool   :: SPrimType BoolT   -- ^ Type of bool   expressions, as singleton.
  TByte   :: SPrimType ByteT   -- ^ Type of byte   expressions, as singleton.
  TShort  :: SPrimType ShortT  -- ^ Type of short  expressions, as singleton.
  TInt    :: SPrimType IntT    -- ^ Type of int    expressions, as singleton.
  TLong   :: SPrimType LongT   -- ^ Type of long   expressions, as singleton.
  TChar   :: SPrimType CharT   -- ^ Type of char   expressions, as singleton.
  TFloat  :: SPrimType FloatT  -- ^ Type of float  expressions, as singleton.
  TDouble :: SPrimType DoubleT -- ^ Type of double expressions, as singleton.

-- | Singleton versions of Type.
data SType (t :: Type) where
  TPrim  :: SPrimType t -> SType (PrimT t)
  TStr   :: SType StringT -- ^ Type of string expressions, as singleton.
  TArray :: SType t -> SType (ArrayT t)

deriving instance Eq   (SPrimType t)
deriving instance Show (SPrimType t)
deriving instance Eq   (SType t)
deriving instance Show (SType t)

-- | dynPType: Convert (primitive type) singleton version to dynamic version.
dynPType :: SPrimType t -> PrimType
dynPType = \case
  TBool   -> BoolT
  TByte   -> ByteT
  TShort  -> ShortT
  TInt    -> IntT
  TLong   -> LongT
  TChar   -> CharT
  TFloat  -> FloatT
  TDouble -> DoubleT

-- | dynType :: Convert (type) singleton version to dynamic version.
dynType :: SType t -> Type
dynType = \case
  TPrim t  -> PrimT  $ dynPType t
  TStr     -> StringT
  TArray t -> ArrayT $ dynType t

-- | TWrap: Existentially quantified version of SType.
-- Exists to allow eqType conversion.
data TWrap where
  -- | Construct a TWrap given a proxy (witness for t) + the type.
  (::~) :: Proxy t -> SType t -> TWrap

-- | eqType :: convert dynamic Type to wrapper of
-- existentially quantified singleton version.
eqType :: Type -> TWrap
eqType = \case
  PrimT t   -> case t of
    BoolT   -> Proxy ::~ TPrim TBool
    ByteT   -> Proxy ::~ TPrim TByte
    ShortT  -> Proxy ::~ TPrim TShort
    IntT    -> Proxy ::~ TPrim TInt
    LongT   -> Proxy ::~ TPrim TLong
    CharT   -> Proxy ::~ TPrim TChar
    FloatT  -> Proxy ::~ TPrim TFloat
    DoubleT -> Proxy ::~ TPrim TDouble
  StringT   -> Proxy ::~ TStr
  ArrayT t  -> case eqType t of
    Proxy ::~ t' -> Proxy ::~ TArray t'

-- | Type level equality...
data a :~: b where
  -- | Refl = types are equal...
  Refl :: a :~: a

-- | Determine if two primitive types are the same at run time.
(=??=) :: SPrimType a -> SPrimType b -> Maybe (a :~: b)
TBool   =??= TBool   = pure Refl
TByte   =??= TByte   = pure Refl
TShort  =??= TShort  = pure Refl
TInt    =??= TInt    = pure Refl
TLong   =??= TLong   = pure Refl
TChar   =??= TChar   = pure Refl
TFloat  =??= TFloat  = pure Refl
TDouble =??= TDouble = pure Refl
_       =??= _       = Nothing

-- | Determine if two types are the same at run time.
(=?=) :: SType a -> SType b -> Maybe (a :~: b)
TPrim a  =?= TPrim b  = a =??= b >>= \Refl -> pure Refl
TStr     =?= TStr     = pure Refl
TArray a =?= TArray b = a =?= b  >>= \Refl -> pure Refl
_        =?= _        = Nothing

--------------------------------------------------------------------------------
-- Literals:
--------------------------------------------------------------------------------

-- | Literal values.
data TLiteral
  = LBool {
      _litBool   :: Bool    -- ^ Literal boolean.
    }
  | LByte {
      _litByte   :: Integer -- ^ Literal byte  (8 bit signed 2s complement).
    }
  | LShort {
      _litShort  :: Integer -- ^ Literal short (16 bit signed 2s complement).
    }
  | LInt {
      _litInt    :: Integer -- ^ Literal int   (32 bit signed 2s complement).
    }
  | LLong {
      _litLong   :: Integer -- ^ Literal long  (64 bit signed 2s complement).
    }
  | LChar {
      _litChar   :: Char    -- ^ Literal char (16 bit unicode character).
    }
  | LStr {
      _litStr    :: String  -- ^ Literal string, list of chars.
    }
  | LFloat {
      _litFloat  :: Double  -- ^ Literal float, 32-bit IEEE 754.
    }
  | LDouble {
      _litDouble :: Double  -- ^ Literal double, 64-bit IEEE 754.
    }
  | Null
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Type synonyms:
--------------------------------------------------------------------------------

type TcIdent           = Ident           TypeCheck
type TcName            = Name            TypeCheck
type TcLValue          = LValue          TypeCheck
type TcVarInit         = VarInit         TypeCheck
type TcArrayInit       = ArrayInit       TypeCheck
type TcExpr            = Expr            TypeCheck
type TcVarDeclId       = VarDeclId       TypeCheck
type TcVarDecl         = VarDecl         TypeCheck
type TcVMType          = VMType          TypeCheck
type TcTypedVVDecl     = TypedVVDecl     TypeCheck
type TcForInit         = ForInit         TypeCheck
type TcSwitchBlock     = SwitchBlock     TypeCheck
type TcSwitchLabel     = SwitchLabel     TypeCheck
type TcBlock           = Block           TypeCheck
type TcStmt            = Stmt            TypeCheck
type TcFormalParam     = FormalParam     TypeCheck
type TcMemberDecl      = MemberDecl      TypeCheck
type TcDecl            = Decl            TypeCheck
type TcClassBody       = ClassBody       TypeCheck
type TcClassDecl       = ClassDecl       TypeCheck
type TcTypeDecl        = TypeDecl        TypeCheck
type TcCompilationUnit = CompilationUnit TypeCheck

--------------------------------------------------------------------------------
-- Type families: literal, types.
--------------------------------------------------------------------------------

type instance XELitT     TypeCheck = TLiteral
type instance XDefType   TypeCheck = Type

--------------------------------------------------------------------------------
-- Type instances, History:
--------------------------------------------------------------------------------

type instance XIdHist   TypeCheck = SRevisor TcIdent           SIdent
type instance XNmHist   TypeCheck = SRevisor TcName            SName
type instance XLVHist   TypeCheck = SRevisor TcLValue          SLValue
type instance XVIHist   TypeCheck = SRevisor TcVarInit         SVarInit
type instance XVDIHist  TypeCheck = SRevisor TcVarDeclId       SVarDeclId
type instance XVDHist   TypeCheck = SRevisor TcVarDecl         SVarDecl
type instance XAIHist   TypeCheck = SRevisor TcArrayInit       SArrayInit
type instance XExprHist TypeCheck = SRevisor TcExpr            SExpr
type instance XVMHist   TypeCheck = SRevisor TcVMType          SVMType
type instance XTvdHist  TypeCheck = SRevisor TcTypedVVDecl     STypedVVDecl
type instance XFiHist   TypeCheck = SRevisor TcForInit         SForInit
type instance XSbHist   TypeCheck = SRevisor TcSwitchBlock     SSwitchBlock
type instance XSlHist   TypeCheck = SRevisor TcSwitchLabel     SSwitchLabel
type instance XSHist    TypeCheck = SRevisor TcStmt            SStmt
type instance XBHist    TypeCheck = SRevisor TcBlock           SBlock
type instance XFpHist   TypeCheck = SRevisor TcFormalParam     SFormalParam
type instance XMdHist   TypeCheck = SRevisor TcMemberDecl      SMemberDecl
type instance XCdHist   TypeCheck = SRevisor TcClassDecl       SClassDecl
type instance XCbHist   TypeCheck = SRevisor TcClassBody       SClassBody
type instance XDeclHist TypeCheck = SRevisor TcDecl            SDecl
type instance XTdHist   TypeCheck = SRevisor TcTypeDecl        STypeDecl
type instance XCUHist   TypeCheck = SRevisor TcCompilationUnit SCompilationUnit

instance Revisable TcIdent where
  type Label TcIdent = String
  type Orig  TcIdent = SIdent
  forget     = fgTop idHist
  revise     = rev idHist
  reviseOrig = revb idHist
  revertL    = revisorME . _idHist

instance Revisable TcName where
  type Label TcName  = String
  type Orig  TcName  = SName
  forgetT    = fgTop nmHist
  forget     = forgetTop . fgFSet nmIds
  revise     = rev nmHist
  reviseOrig = revb nmHist
  revertL    = revisorME . _nmHist

instance Revisable TcLValue where
  type Label TcLValue = String
  type Orig  TcLValue = SLValue
  forgetT    = fgTop lvHist
  forget     = forgetTop . fgSet lvId . fgSet lvExpr . fgFSet lvExprs
  revise     = rev lvHist
  reviseOrig = revb lvHist
  revertL    = revisorME . _lvHist

instance Revisable TcVarInit where
  type Label TcVarInit = String
  type Orig  TcVarInit = SVarInit
  forgetT    = fgTop viHist
  forget     = forgetTop . fgSet viExpr . fgSet viArrInit
  revise     = rev viHist
  reviseOrig = revb viHist
  revertL    = revisorME . _viHist

instance Revisable TcArrayInit where
  type Label TcArrayInit = String
  type Orig  TcArrayInit = SArrayInit
  forgetT    = fgTop aiHist
  forget     = forgetTop . fgFSet aiVIs
  revise     = rev aiHist
  reviseOrig = revb aiHist
  revertL    = revisorME . _aiHist

instance Revisable TcExpr where
  type Label TcExpr = String
  type Orig  TcExpr = SExpr
  forgetTop  = fgTop eHist
  forgetT    = forgetTop . fgSet eExpr . fgSet eCond . fgSet eExpI . fgSet eExpE
                         . fgSet eLeft . fgSet eRight . fgFSet eExprs
  forget     = forgetT   . fgSet eLValue . fgSet eName . fgSet eAInit
  revise     = rev eHist
  reviseOrig = revb eHist
  revertL    = revisorME . _eHist

instance Revisable TcVarDeclId where
  type Label TcVarDeclId = String
  type Orig  TcVarDeclId = SVarDeclId
  forgetTop  = fgTop vdiHist
  forget     = forgetTop . fgSet vdiIdent
  revise     = rev vdiHist
  reviseOrig = revb vdiHist
  revertL    = revisorME . _vdiHist

instance Revisable TcVarDecl where
  type Label TcVarDecl = String
  type Orig  TcVarDecl = SVarDecl
  forgetTop  = fgTop vdHist
  forget     = forgetTop . fgSet vdVDI . fgFSet vdVInit
  revise     = rev vdHist
  reviseOrig = revb vdHist
  revertL    = revisorME . _vdHist

instance Revisable TcForInit where
  type Label TcForInit = String
  type Orig  TcForInit = SForInit
  forgetTop  = fgTop fiHist
  forget     = forgetTop . fgFSet fiExprs . fgSet fiVars
  revise     = rev fiHist
  reviseOrig = revb fiHist
  revertL    = revisorME . _fiHist

instance Revisable TcVMType where
  type Label TcVMType = String
  type Orig  TcVMType = SVMType
  forgetTop  = fgTop vmHist
  forget     = forgetTop
  revise     = rev vmHist
  reviseOrig = revb vmHist
  revertL    = revisorME . _vmHist

instance Revisable TcTypedVVDecl where
  type Label TcTypedVVDecl = String
  type Orig  TcTypedVVDecl = STypedVVDecl
  forgetTop  = fgTop tvdHist
  forget     = forgetTop . fgFSet tvdVDecls
  revise     = rev tvdHist
  reviseOrig = revb tvdHist
  revertL    = revisorME . _tvdHist

instance Revisable TcSwitchLabel where
  type Label TcSwitchLabel = String
  type Orig  TcSwitchLabel = SSwitchLabel
  forgetTop  = fgTop slHist
  forget     = forgetTop . fgSet slExpr
  revise     = rev slHist
  reviseOrig = revb slHist
  revertL    = revisorME . _slHist

instance Revisable TcSwitchBlock where
  type Label TcSwitchBlock = String
  type Orig  TcSwitchBlock = SSwitchBlock
  forgetTop  = fgTop sbHist
  forget     = forgetTop . fgSet sbLab . fgSet sbBlock
  revise     = rev sbHist
  reviseOrig = revb sbHist
  revertL    = revisorME . _sbHist

instance Revisable TcBlock where
  type Label TcBlock = String
  type Orig  TcBlock = SBlock
  forgetTop  = fgTop bHist
  forget     = forgetTop . fgFSet bStmts
  revise     = rev bHist
  reviseOrig = revb bHist
  revertL    = revisorME . _bHist

instance Revisable TcStmt where
  type Label TcStmt = String
  type Orig  TcStmt = SStmt
  forgetTop  = fgTop sHist
  forgetT    = forgetTop . fgSet sSi . fgSet sSe
  forget     = forgetT . fgSet sBlock . fgSet sExpr . fgFSet sForInit .
                         fgFSet sForECond . fgSet sForVar . fgFSet sSwiBlock .
                         (sForEPost %~ (fmap . fmap) forget) .
                         fgSet sVDecl . fgSet sForVMTy
  revise     = rev sHist
  reviseOrig = revb sHist
  revertL    = revisorME . _sHist

instance Revisable TcFormalParam where
  type Label TcFormalParam = String
  type Orig  TcFormalParam = SFormalParam
  forgetTop  = fgTop fpHist
  forget     = forgetTop . fgSet fpType . fgSet fpVDI
  revise     = rev fpHist
  reviseOrig = revb fpHist
  revertL    = revisorME . _fpHist

instance Revisable TcMemberDecl where
  type Label TcMemberDecl = String
  type Orig  TcMemberDecl = SMemberDecl
  forgetTop  = fgTop mdHist
  forget     = forgetTop . fgSet mdName . fgFSet mdParams . fgSet mdBlock
  revise     = rev mdHist
  reviseOrig = revb mdHist
  revertL    = revisorME . _mdHist

instance Revisable TcDecl where
  type Label TcDecl = String
  type Orig  TcDecl = SDecl
  forgetTop  = fgTop declHist
  forget     = forgetTop . fgSet declMem
  revise     = rev declHist
  reviseOrig = revb declHist
  revertL    = revisorME . _declHist

instance Revisable TcClassBody where
  type Label TcClassBody = String
  type Orig  TcClassBody = SClassBody
  forgetTop  = fgTop cbHist
  forget     = forgetTop . fgFSet cbDecls
  revise     = rev cbHist
  reviseOrig = revb cbHist
  revertL    = revisorME . _cbHist

instance Revisable TcClassDecl where
  type Label TcClassDecl = String
  type Orig  TcClassDecl = SClassDecl
  forgetTop  = fgTop cdHist
  forget     = forgetTop . fgSet cdId . fgSet cdBody
  revise     = rev cdHist
  reviseOrig = revb cdHist
  revertL    = revisorME . _cdHist

instance Revisable TcTypeDecl where
  type Label TcTypeDecl = String
  type Orig  TcTypeDecl = STypeDecl
  forgetTop  = fgTop tdHist
  forget     = forgetTop . fgSet tdClass
  revise     = rev tdHist
  reviseOrig = revb tdHist
  revertL    = revisorME . _tdHist

instance Revisable TcCompilationUnit where
  type Label TcCompilationUnit = String
  type Orig  TcCompilationUnit = SCompilationUnit
  forgetTop  = fgTop cuHist
  forget     = forgetTop . fgFSet cuTDecls
  revise     = rev cuHist
  reviseOrig = revb cuHist
  revertL    = revisorME . _cuHist

--------------------------------------------------------------------------------
-- Type families: extra stuff.
--------------------------------------------------------------------------------

data Purity = Constant | Pure | Impure
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

type RType = Maybe Type

data ExprExt = EExt
  { _eePurity :: Purity
  , _eeType   :: RType
  } deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data VarExt = VExt
  { _veWrites :: Integer
  , _veReads  :: Integer
  } deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data LVExt = LVExt
  { _leExpr :: ExprExt
  , _leVar  :: VarExt
  } deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

$(deriveLens [''TLiteral, ''Purity, ''ExprExt, ''VarExt, ''LVExt])

type instance XLVName    TypeCheck = LVExt
type instance XLVArr     TypeCheck = LVExt
type instance XExpr      TypeCheck = Void
type instance XELit      TypeCheck = ExprExt
type instance XEVar      TypeCheck = ExprExt
type instance XECast     TypeCheck = ExprExt
type instance XECond     TypeCheck = ExprExt
type instance XEAssign   TypeCheck = ExprExt
type instance XEOAssign  TypeCheck = ExprExt
type instance XENum      TypeCheck = ExprExt
type instance XECmp      TypeCheck = ExprExt
type instance XELog      TypeCheck = ExprExt
type instance XENot      TypeCheck = ExprExt
type instance XEStep     TypeCheck = ExprExt
type instance XEBCompl   TypeCheck = ExprExt
type instance XEPlus     TypeCheck = ExprExt
type instance XEMinus    TypeCheck = ExprExt
type instance XEMApp     TypeCheck = ExprExt
type instance XEArrNew   TypeCheck = ExprExt
type instance XESysOut   TypeCheck = ExprExt

type instance XStmt      TypeCheck = Void
type instance XSEmpty    TypeCheck = Purity
type instance XSBlock    TypeCheck = Purity
type instance XSExpr     TypeCheck = Purity
type instance XSVars     TypeCheck = Purity
type instance XSReturn   TypeCheck = Purity
type instance XSVReturn  TypeCheck = Purity
type instance XSIf       TypeCheck = Purity
type instance XSIfElse   TypeCheck = Purity
type instance XSWhile    TypeCheck = Purity
type instance XSDo       TypeCheck = Purity
type instance XSForB     TypeCheck = Purity
type instance XSForE     TypeCheck = Purity
type instance XSContinue TypeCheck = Purity
type instance XSBreak    TypeCheck = Purity
type instance XSSwitch   TypeCheck = Purity
type instance XBlock     TypeCheck = Purity
type instance XMethDecl  TypeCheck = Purity
type instance XFormPar   TypeCheck = VarExt