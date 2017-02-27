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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric
  , TypeFamilies, FlexibleInstances #-}

module Core.Start.AST where

import Data.Void (Void)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import qualified Language.Java.Syntax as S

import Core.Common.History
import Core.Common.AST
import Core.Start.Phase as X

import Control.Lens ((%~))

--------------------------------------------------------------------------------
-- Types:
--------------------------------------------------------------------------------

-- | Primitive types.
data PrimType
  = BoolT   -- ^ Type of bool   values and expressions.
  | ByteT   -- ^ Type of byte   values and expressions.
  | ShortT  -- ^ Type of short  values and expressions.
  | IntT    -- ^ Type of int    values and expressions.
  | LongT   -- ^ Type of long   values and expressions.
  | CharT   -- ^ Type of char   values and expressions.
  | FloatT  -- ^ Type of float  values and expressions.
  | DoubleT -- ^ Type of double values and expressions.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | All possible types that value / expression can be of.
data Type
  = PrimT {
      _tPrim :: PrimType -- ^ A primitive type.
    }
  | StringT              -- ^ A String type.
  | ArrayT {
      _tType :: Type     -- ^ An array type of some other type.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Literals:
--------------------------------------------------------------------------------

-- | Literal values.
data Literal
  = Int {
      _litI :: Integer -- ^ Literal integer.
    }
  | Word {
      _litI :: Integer -- ^ Literal word.
    }
  | Float {
      _litD :: Double  -- ^ Literal float.
    }
  | Double {
      _litD :: Double  -- ^ Literal double.
    }
  | Boolean {
      _litB :: Bool    -- ^ Literal boolean.
    }
  | Char {
      _litC :: Char    -- ^ Literal char.
    }
  | String {
      _litS :: String  -- ^ Literal String.
    }
  | Null               -- ^ Literal null.
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Type synonyms:
--------------------------------------------------------------------------------

type SIdent           = Ident           Start
type SName            = Name            Start
type SLValue          = LValue          Start
type SVarInit         = VarInit         Start
type SArrayInit       = ArrayInit       Start
type SExpr            = Expr            Start
type SVarDeclId       = VarDeclId       Start
type SVarDecl         = VarDecl         Start
type SVMType          = VMType          Start
type STypedVVDecl     = TypedVVDecl     Start
type SForInit         = ForInit         Start
type SSwitchBlock     = SwitchBlock     Start
type SSwitchLabel     = SwitchLabel     Start
type SBlock           = Block           Start
type SStmt            = Stmt            Start
type SFormalParam     = FormalParam     Start
type SMemberDecl      = MemberDecl      Start
type SDecl            = Decl            Start
type SClassBody       = ClassBody       Start
type SClassDecl       = ClassDecl       Start
type STypeDecl        = TypeDecl        Start
type SCompilationUnit = CompilationUnit Start

--------------------------------------------------------------------------------
-- Type families: literal, types.
--------------------------------------------------------------------------------

type instance XELitT     Start = Literal
type instance XDefType   Start = Type

--------------------------------------------------------------------------------
-- Type instances, History:
--------------------------------------------------------------------------------

type SynTVVDecl = ([S.Modifier], S.Type, [S.VarDecl])
type SynVMType  = ([S.Modifier], S.Type)

type instance XIdHist   Start = SRevisor SIdent           S.Ident
type instance XNmHist   Start = SRevisor SName            S.Name
type instance XLVHist   Start = SRevisor SLValue          S.Lhs
type instance XVIHist   Start = SRevisor SVarInit         S.VarInit
type instance XVDIHist  Start = SRevisor SVarDeclId       S.VarDeclId
type instance XVDHist   Start = SRevisor SVarDecl         S.VarDecl
type instance XAIHist   Start = SRevisor SArrayInit       S.ArrayInit
type instance XExprHist Start = SRevisor SExpr            S.Exp
type instance XVMHist   Start = SRevisor SVMType          SynVMType
type instance XTvdHist  Start = SRevisor STypedVVDecl     SynTVVDecl
type instance XFiHist   Start = SRevisor SForInit         S.ForInit
type instance XSbHist   Start = SRevisor SSwitchBlock     S.SwitchBlock
type instance XSlHist   Start = SRevisor SSwitchLabel     S.SwitchLabel
type instance XSHist    Start = SRevisor SStmt            S.BlockStmt
type instance XBHist    Start = SRevisor SBlock           S.Block
type instance XFpHist   Start = SRevisor SFormalParam     S.FormalParam
type instance XMdHist   Start = SRevisor SMemberDecl      S.MemberDecl
type instance XCdHist   Start = SRevisor SClassDecl       S.ClassDecl
type instance XCbHist   Start = SRevisor SClassBody       S.ClassBody
type instance XDeclHist Start = SRevisor SDecl            S.Decl
type instance XTdHist   Start = SRevisor STypeDecl        S.TypeDecl
type instance XCUHist   Start = SRevisor SCompilationUnit S.CompilationUnit

instance Revisable SIdent where
  type Label SIdent = String
  type Orig  SIdent = S.Ident
  forget     = fgTop idHist
  revise     = rev idHist
  reviseOrig = revb idHist
  revertL    = revisorME . _idHist

instance Revisable SName where
  type Label SName  = String
  type Orig  SName  = S.Name
  forgetT    = fgTop nmHist
  forget     = forgetTop . fgFSet nmIds
  revise     = rev nmHist
  reviseOrig = revb nmHist
  revertL    = revisorME . _nmHist

instance Revisable SLValue where
  type Label SLValue = String
  type Orig  SLValue = S.Lhs
  forgetT    = fgTop lvHist
  forget     = forgetTop . fgSet lvId . fgSet lvExpr . fgFSet lvExprs
  revise     = rev lvHist
  reviseOrig = revb lvHist
  revertL    = revisorME . _lvHist

instance Revisable SVarInit where
  type Label SVarInit = String
  type Orig  SVarInit = S.VarInit
  forgetT    = fgTop viHist
  forget     = forgetTop . fgSet viExpr . fgSet viArrInit
  revise     = rev viHist
  reviseOrig = revb viHist
  revertL    = revisorME . _viHist

instance Revisable SArrayInit where
  type Label SArrayInit = String
  type Orig  SArrayInit = S.ArrayInit
  forgetT    = fgTop aiHist
  forget     = forgetTop . fgFSet aiVIs
  revise     = rev aiHist
  reviseOrig = revb aiHist
  revertL    = revisorME . _aiHist

instance Revisable SExpr where
  type Label SExpr = String
  type Orig  SExpr = S.Exp
  forgetTop  = fgTop eHist
  forgetT    = forgetTop . fgSet eExpr . fgSet eCond . fgSet eExpI . fgSet eExpE
                         . fgSet eLeft . fgSet eRight . fgFSet eExprs
  forget     = forgetT   . fgSet eLValue . fgSet eName . fgSet eAInit
  revise     = rev eHist
  reviseOrig = revb eHist
  revertL    = revisorME . _eHist

instance Revisable SVarDeclId where
  type Label SVarDeclId = String
  type Orig  SVarDeclId = S.VarDeclId
  forgetTop  = fgTop vdiHist
  forget     = forgetTop . fgSet vdiIdent
  revise     = rev vdiHist
  reviseOrig = revb vdiHist
  revertL    = revisorME . _vdiHist

instance Revisable SVarDecl where
  type Label SVarDecl = String
  type Orig  SVarDecl = S.VarDecl
  forgetTop  = fgTop vdHist
  forget     = forgetTop . fgSet vdVDI . fgFSet vdVInit
  revise     = rev vdHist
  reviseOrig = revb vdHist
  revertL    = revisorME . _vdHist

instance Revisable SForInit where
  type Label SForInit = String
  type Orig  SForInit = S.ForInit
  forgetTop  = fgTop fiHist
  forget     = forgetTop . fgFSet fiExprs . fgSet fiVars
  revise     = rev fiHist
  reviseOrig = revb fiHist
  revertL    = revisorME . _fiHist

instance Revisable SVMType where
  type Label SVMType = String
  type Orig  SVMType = SynVMType
  forgetTop  = fgTop vmHist
  forget     = forgetTop
  revise     = rev vmHist
  reviseOrig = revb vmHist
  revertL    = revisorME . _vmHist

instance Revisable STypedVVDecl where
  type Label STypedVVDecl = String
  type Orig  STypedVVDecl = SynTVVDecl
  forgetTop  = fgTop tvdHist
  forget     = forgetTop . fgFSet tvdVDecls
  revise     = rev tvdHist
  reviseOrig = revb tvdHist
  revertL    = revisorME . _tvdHist

instance Revisable SSwitchLabel where
  type Label SSwitchLabel = String
  type Orig  SSwitchLabel = S.SwitchLabel
  forgetTop  = fgTop slHist
  forget     = forgetTop . fgSet slExpr
  revise     = rev slHist
  reviseOrig = revb slHist
  revertL    = revisorME . _slHist

instance Revisable SSwitchBlock where
  type Label SSwitchBlock = String
  type Orig  SSwitchBlock = S.SwitchBlock
  forgetTop  = fgTop sbHist
  forget     = forgetTop . fgSet sbLab . fgSet sbBlock
  revise     = rev sbHist
  reviseOrig = revb sbHist
  revertL    = revisorME . _sbHist

instance Revisable SBlock where
  type Label SBlock = String
  type Orig  SBlock = S.Block
  forgetTop  = fgTop bHist
  forget     = forgetTop . fgFSet bStmts
  revise     = rev bHist
  reviseOrig = revb bHist
  revertL    = revisorME . _bHist

instance Revisable SStmt where
  type Label SStmt = String
  type Orig  SStmt = S.BlockStmt
  forgetTop  = fgTop sHist
  forgetT    = forgetTop . fgSet sSi . fgSet sSe
  forget     = forgetT . fgSet sBlock . fgSet sExpr . fgFSet sForInit .
                         fgFSet sForECond . fgSet sForVar . fgFSet sSwiBlock .
                         (sForEPost %~ (fmap . fmap) forget) .
                         fgSet sVDecl . fgSet sForVMTy
  revise     = rev sHist
  reviseOrig = revb sHist
  revertL    = revisorME . _sHist

instance Revisable SFormalParam where
  type Label SFormalParam = String
  type Orig  SFormalParam = S.FormalParam
  forgetTop  = fgTop fpHist
  forget     = forgetTop . fgSet fpType . fgSet fpVDI
  revise     = rev fpHist
  reviseOrig = revb fpHist
  revertL    = revisorME . _fpHist

instance Revisable SMemberDecl where
  type Label SMemberDecl = String
  type Orig  SMemberDecl = S.MemberDecl
  forgetTop  = fgTop mdHist
  forget     = forgetTop . fgSet mdName . fgFSet mdParams . fgSet mdBlock
  revise     = rev mdHist
  reviseOrig = revb mdHist
  revertL    = revisorME . _mdHist

instance Revisable SDecl where
  type Label SDecl = String
  type Orig  SDecl = S.Decl
  forgetTop  = fgTop declHist
  forget     = forgetTop . fgSet declMem
  revise     = rev declHist
  reviseOrig = revb declHist
  revertL    = revisorME . _declHist

instance Revisable SClassBody where
  type Label SClassBody = String
  type Orig  SClassBody = S.ClassBody
  forgetTop  = fgTop cbHist
  forget     = forgetTop . fgFSet cbDecls
  revise     = rev cbHist
  reviseOrig = revb cbHist
  revertL    = revisorME . _cbHist

instance Revisable SClassDecl where
  type Label SClassDecl = String
  type Orig  SClassDecl = S.ClassDecl
  forgetTop  = fgTop cdHist
  forget     = forgetTop . fgSet cdId . fgSet cdBody
  revise     = rev cdHist
  reviseOrig = revb cdHist
  revertL    = revisorME . _cdHist

instance Revisable STypeDecl where
  type Label STypeDecl = String
  type Orig  STypeDecl = S.TypeDecl
  forgetTop  = fgTop tdHist
  forget     = forgetTop . fgSet tdClass
  revise     = rev tdHist
  reviseOrig = revb tdHist
  revertL    = revisorME . _tdHist

instance Revisable SCompilationUnit where
  type Label SCompilationUnit = String
  type Orig  SCompilationUnit = S.CompilationUnit
  forgetTop  = fgTop cuHist
  forget     = forgetTop . fgFSet cuTDecls
  revise     = rev cuHist
  reviseOrig = revb cuHist
  revertL    = revisorME . _cuHist

--------------------------------------------------------------------------------
-- Type families: extra stuff.
--------------------------------------------------------------------------------

type instance XLVName    Start = ()
type instance XLVArr     Start = ()
type instance XExpr      Start = Void
type instance XELit      Start = ()
type instance XEVar      Start = ()
type instance XECast     Start = ()
type instance XECond     Start = ()
type instance XEAssign   Start = ()
type instance XEOAssign  Start = ()
type instance XENum      Start = ()
type instance XECmp      Start = ()
type instance XELog      Start = ()
type instance XENot      Start = ()
type instance XEStep     Start = ()
type instance XEBCompl   Start = ()
type instance XEPlus     Start = ()
type instance XEMinus    Start = ()
type instance XEMApp     Start = ()
type instance XEArrNew   Start = ()
type instance XESysOut   Start = ()
type instance XStmt      Start = Void
type instance XSEmpty    Start = ()
type instance XSBlock    Start = ()
type instance XSExpr     Start = ()
type instance XSVars     Start = ()
type instance XSReturn   Start = ()
type instance XSVReturn  Start = ()
type instance XSIf       Start = ()
type instance XSIfElse   Start = ()
type instance XSWhile    Start = ()
type instance XSDo       Start = ()
type instance XSForB     Start = ()
type instance XSForE     Start = ()
type instance XSContinue Start = ()
type instance XSBreak    Start = ()
type instance XSSwitch   Start = ()
type instance XBlock     Start = ()
type instance XMethDecl  Start = ()
type instance XFormPar   Start = ()