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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving
  , ConstraintKinds, TypeFamilies, UndecidableInstances, TemplateHaskell #-}

module Core.Common.AST where

import Data.Function (on)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import GHC.Types (Constraint)

import Class.Approx
import Core.Common.TH

--------------------------------------------------------------------------------
-- Names and identifiers:
--------------------------------------------------------------------------------

-- | Ident: Identifier of a variable, class, method, etc.
data Ident p = Ident
  { _idHist :: XIdHist p -- ^ The history of the identifier.
  , _idId   :: String    -- ^ The actual name of the identifier.
  }

-- | Name: Qualified identifier.
data Name p = Name
  { _nmHist :: XNmHist p -- ^ The history of the name.
  , _nmIds  :: [Ident p] -- ^ Identifier parts of the name.
  }

--------------------------------------------------------------------------------
-- Operators:
--------------------------------------------------------------------------------

-- NumOp: Numeric operators, both in assignment and as binary operators.
data NumOp
  = Add     -- ^ Numeric operator for Addition.
  | Sub     -- ^ Numeric operator for Subtraction.
  | Mul     -- ^ Numeric operator for Multiplication.
  | Div     -- ^ Numeric operator for Division.
  | Rem     -- ^ Numeric operator for Remainder (not modulo).
  | LShift  -- ^ Numeric operator for Left shift.
  | RShift  -- ^ Numeric operator for Right shift.
  | RRShift -- ^ Numeric operator for Unsigned right shift.
  | And     -- ^ Numeric operator for Bitwise And.
  | Xor     -- ^ Numeric operator for Bitwise Xor.
  | Or      -- ^ Numeric operator for Bitwise Or.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- CmpOp: (binary) comparison operators, result is always boolean typed.
data CmpOp
  = EQ -- ^ Comparison operator for Equality (==).
  | NE -- ^ Comparison operator for Inequality (!=).
  | LT -- ^ Comparison operator for Less than (<).
  | GT -- ^ Comparison operator for Greater than (>).
  | LE -- ^ Comparison operator for Less than / Equal to (<=).
  | GE -- ^ Comparison operator for Greater than / Equal to (>=).
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- LogOp: (binary) logical operators, operands and results are boolean typed.
data LogOp
  = LAnd -- ^ Logical conjunction \land (LaTeX) operator.
  | LOr  -- ^ Logical disjunction \lor  (LaTeX) operator.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | StepOp: Unary stepping operators, operand must be of a numeric type.
data StepOp
  = PostInc -- ^ Unary operator for Post Incrementation (i++).
  | PostDec -- ^ Unary operator for Post Decrementation (i--).
  | PreInc  -- ^ Unary operator for Pre Incrementation (++i).
  | PreDec  -- ^ Unary operator for Pre Incrementation (--i).
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- lvalues:
--------------------------------------------------------------------------------

-- | LValue: An LValue, as opposed to RValue, is an "expression" that can
-- occur on the left hand side of an assignment. I.e: it models what it means
-- to be a location in memory.
data LValue p
  = LVName {
      _lvHist :: XLVHist p -- ^ History of an LValue.
    , _lvXLVN :: XLVName p -- ^ Extra fields of LVName for phase p.
    , _lvId   :: Ident p   -- ^ Simple variable identifier.
    }
  | LVArray {
      _lvHist :: XLVHist p -- ^ History of an LValue.
    , _lvXLVA :: XLVArr p  -- ^ Extra fields of LVArray for phase p.
    , _lvExpr  :: Expr p   -- ^ An expression yielding an array.
    , _lvExprs :: [Expr p] -- ^ Indexing expressions, must be castable to int.
    }

--------------------------------------------------------------------------------
-- Variable & Array initialization:
--------------------------------------------------------------------------------

-- | VarInit: initializer of variables.
data VarInit p
  = InitExpr {
      _viHist    :: XVIHist p   -- ^ History of a VarInit.
    , _viExpr    :: Expr p      -- ^ Initializer expression.
    }
  | InitArr {
      _viHist    :: XVIHist p   -- ^ History of a VarInit.
    , _viArrInit :: ArrayInit p -- ^ Initializing an array.
    }

-- | ArrayInit: Initializer of array creation (new T[]...) expressions.
data ArrayInit p = ArrayInit
  {
     _aiHist :: XAIHist p   -- ^ History of a ArrayInit.
  ,  _aiVIs  :: [VarInit p] -- ^ List of initializers for each element in array.
  }

--------------------------------------------------------------------------------
-- Expressions:
--------------------------------------------------------------------------------

-- | Expr: the type of expressions, phase-indexed by p.
data Expr p
  = EExpr {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXExpr    :: XExpr p     -- ^ Extra constructors for a phase p of Expr p.
    }
  | ELit {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXLit     :: XELit p     -- ^ Extra fields for a phase p of ELit.
    , _eLit      :: XELitT p    -- ^ Literal of an ELit, depends on index p.
  }
  | EVar  {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXVar     :: XEVar p     -- ^ Extra fields for a phase p of EVar.
    , _eLValue   :: LValue p    -- ^ LValue of a variable reference.
  }
  | ECast {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXCast    :: XECast p    -- ^ Extra fields for a phase p of ECast.
    , _eDefT     :: XDefType p  -- ^ To-cast type of an ECast, indexed by p.
    , _eExpr     :: Expr p      -- ^ Expr to cast to other type.
    }
  | ECond {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXCond    :: XECond p    -- ^ Extra fields for a phase p of ECond.
    , _eCond     :: Expr p      -- ^ Condition expr of a ternary expr.
    , _eExpI     :: Expr p      -- ^ Expr to eval on truth of condition expr.
    , _eExpE     :: Expr p      -- ^ Expr to eval on falsehood of condition expr.
  }
  | EAssign {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXAssign  :: XEAssign p  -- ^ Extra fields for a phase p of EAssign.
    , _eLValue   :: LValue p    -- ^ Variable reference to assign to.
    , _eExpr     :: Expr p      -- ^ Expression to assign to variable reference.
  }
  | EOAssign {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXOAssign :: XEOAssign p -- ^ Extra fields for a phase p of EOAssign.
    , _eLValue   :: LValue p    -- ^ Variable reference to assign to.
    , _eNumOp    :: NumOp       -- ^ Assignment operator.
    , _eExpr     :: Expr p      -- ^ Expression to modify variable ref with.
    }
  | ENum {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXNum     :: XENum p     -- ^ Extra fields for a phase p of ENum.
    , _eNumOp    :: NumOp       -- ^ Numeric operation to compute.
    , _eLeft     :: Expr p      -- ^ Left operand.
    , _eRight    :: Expr p      -- ^ Right operand.
  }
  | ECmp {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXCmp     :: XECmp p     -- ^ Extra fields for a phase p of ECmp.
    , _eCmpOp    :: CmpOp       -- ^ Comparison operator to compute.
    , _eLeft     :: Expr p      -- ^ Left operand.
    , _eRight    :: Expr p      -- ^ Right operand.
  }
  | ELog {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXLog     :: XELog p     -- ^ Extra fields for a phase p of ELog.
    , _eLogOp    :: LogOp       -- ^ Logical operator to compute.
    , _eLeft     :: Expr p      -- ^ Left operand.
    , _eRight    :: Expr p      -- ^ Right operand.
  }
  | ENot {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXNot     :: XENot p     -- ^ Extra fields for a phase p of ENot.
    , _eExpr     :: Expr p      -- ^ Expression to compute logical not on.
    }
  | EStep {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXStep    :: XEStep p    -- ^ Extra fields for a phase p of EStep.
    , _eStepOp   :: StepOp      -- ^ Stepping operator.
    , _eExpr     :: Expr p      -- ^ Expression to step, must be variable ref.
    }
  | EBCompl {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXBCompl  :: XEBCompl p  -- ^ Extra fields for a phase p of EBCompl.
    , _eExpr     :: Expr p      -- ^ Expr to compute bitwise complement on.
    }
  | EPlus {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXPlus    :: XEPlus p    -- ^ Extra fields for a phase p of EPlus.
    , _eExpr     :: Expr p      -- ^ Expr to compute unary plus on.
    }
  | EMinus {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXMinus   :: XEMinus p   -- ^ Extra fields for a phase p of EMinus.
    , _eExpr     :: Expr p      -- ^ Expr to compute unary negation of.
    }
  | EMApp {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXMApp    :: XEMApp p    -- ^ Extra fields for a phase p of EMApp.
    , _eName     :: Name p      -- ^ Name of method to apply.
    , _eExprs    :: [Expr p]
    }
  | EArrNew {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXArrNew  :: XEArrNew p  -- ^ Extra fields for a phase p of EArrNew(I).
    , _eDefT     :: XDefType p  -- ^ Base-type of array to construct,
                                -- phase-indexed by p.
    , _eExprs    :: [Expr p]    -- ^ Expressions to compute lengths with.
    , _eEDims    :: Integer     -- ^ Number of extra dimensions.
    }
  | EArrNewI {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXArrNew  :: XEArrNew p  -- ^ Extra fields for a phase p of EArrNew(I).
    , _eDefT     :: XDefType p  -- ^ Base-type of array to construct.
    , _eDims     :: Integer     -- ^ Number of dimensions.
    , _eAInit    :: ArrayInit p -- ^ Array initializer.
    }
  | ESysOut {
      _eHist     :: XExprHist p -- ^ The history of an (Expr p).
    , _eXSysOut  :: XESysOut p  -- ^ Extra fields for a phase p of ESysOut.
    , _eExpr     :: Expr p      -- ^ Expression to print out.
    }

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

-- | VarDeclId: identifier of variable declarations.
data VarDeclId p
  = VarDId {
      _vdiHist  :: XVDIHist p -- ^ History of VarDeclId.
    , _vdiIdent :: Ident p    -- ^ Identifier of the variable.
    }
  | VarDArr {
      _vdiHist  :: XVDIHist p -- ^ History of VarDeclId.
    , _vdiIdent :: Ident p    -- ^ Identifier of the variable.
    , _vdiDimen :: Integer    -- ^ Dimensionality of the variable.
    }

-- | VarDecl: variable declaration (id + initializer).
data VarDecl p = VarDecl
  {
    _vdHist  :: XVDHist p         -- ^ History of VarDecl.
  , _vdVDI   :: VarDeclId p       -- ^ Identifier part of variable.
  , _vdVInit :: Maybe (VarInit p) -- ^ Potential variable initializer.
  }

-- | VarMod: A variable can either be declared as final, or not.
data VarMod = VMFinal | VMNormal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | VMType: VarMod + Type.
data VMType p = VMType
  { _vmHist :: XVMHist p  -- ^ History of VMType.
  , _vmMod  :: VarMod     -- ^ Modifier of type (final / not).
  , _vmType :: XDefType p -- ^ Base type of the VMType.
  }

-- | TypedVVDecl: Typed Variable declarations.
data TypedVVDecl p = TypedVVDecl
  {
    _tvdHist   :: XTvdHist p  -- ^ History of TypedVVDecl.
  , _tvdType   :: VMType p    -- ^ Type of variable declarations.
  , _tvdVDecls :: [VarDecl p] -- ^ List of variable declarations.
  }

-- | ForInit: For loop initializer for normal for loops.
data ForInit p
  = FIVars {
      _fiHist  :: XFiHist p     -- ^ History of ForInit.
    , _fiVars  :: TypedVVDecl p -- ^ For loop variable declarations.
    }
  | FIExprs {
      _fiHist  :: XFiHist p     -- ^ History of ForInit.
    , _fiExprs :: [Expr p]      -- ^ For loop initial variable expressions.
    }

-- | SwitchBlock: One match arm of a switch block.
data SwitchBlock p = SwitchBlock
  { _sbHist  :: XSbHist p     -- ^ History of the SwitchBlock.
  , _sbLab   :: SwitchLabel p -- ^ SwitchLabel of the block.
  , _sbBlock :: Block p       -- ^ Block of the SwitchBlock.
  }

-- | SwitchLabel (case).
data SwitchLabel p
  = SwitchCase {
      _slHist :: XSlHist p -- ^ History of SwitchLabel.
    , _slExpr :: Expr p    -- ^ Case expression.
    }
  | Default {              -- ^ Default case.
      _slHist :: XSlHist p -- ^ History of SwitchLabel.
    }

-- | Block of statements.
data Block p = Block
  { _bHist   :: XBHist p -- ^ History of the Block.
  , _bXBlock :: XBlock p -- ^ Extra fields for phase p of Block.
  , _bStmts  :: [Stmt p] -- ^ List of statements in the block.
  }

-- | Stmt: A statement, unlike expressions, it has no value.
data Stmt p
  = SStmt {
      _sHist       :: XSHist p -- ^ History of the statement.
    , _sXStmt      :: XStmt p  -- ^ Extra constructor of Stmt p.
    }
  | SEmpty {
      _sHist       :: XSHist p  -- ^ History of the statement.
    , _sXSEmpty    :: XSEmpty p -- ^ Extra field of SEmpty.
    }
  | SBlock {
      _sHist       :: XSHist p  -- ^ History of the statement.
    , _sXSBlock    :: XSBlock p -- ^ Extra field of SBlock.
    , _sBlock      :: Block p   -- ^ The block of the SBlock.
    }
  | SExpr {
      _sHist       :: XSHist p -- ^ History of the statement.
    , _sXSExpr     :: XSExpr p -- ^ Extra field of SExpr.
    , _sExpr       :: Expr p   -- ^ The Expr of the SExpr.
    }
  | SVars {
      _sHist       :: XSHist p      -- ^ History of the statement.
    , _sXSVars     :: XSVars p      -- ^ Extra field of SVars.
    , _sVDecl      :: TypedVVDecl p -- ^ The variable declarations of SVars.
    }
  | SReturn {
      _sHist       :: XSHist p   -- ^ History of the statement.
    , _sXSReturn   :: XSReturn p -- ^ Extra field of SReturn.
    , _sExpr       :: Expr p     -- ^ The Expression to return.
    }
  | SVReturn {
      _sHist       :: XSHist p    -- ^ History of the statement.
    , _sXSVReturn  :: XSVReturn p -- ^ Extra field of SVReturn.
    }
  | SIf {
      _sHist       :: XSHist p -- ^ History of the statement.
    , _sXSIf       :: XSIf p   -- ^ Extra field of SIf.
    , _sExpr       :: Expr p   -- ^ Condition expression of SIf.
    , _sSi         :: Stmt p   -- ^ Statement to execute on truth.
    }
  | SIfElse {
      _sHist       :: XSHist p   -- ^ History of the statement.
    , _sXSIfElse   :: XSIfElse p -- ^ Extra field of SIfElse.
    , _sExpr       :: Expr p     -- ^ Condition expression of SIf.
    , _sSi         :: Stmt p     -- ^ Statement to execute on truth.
    , _sSe         :: Stmt p     -- ^ Statement to execute on falsehood.
    }
  | SWhile {
      _sHist       :: XSHist p  -- ^ History of the statement.
    , _sXSWhile    :: XSWhile p -- ^ Extra field of SWhile.
    , _sExpr       :: Expr p    -- ^ Condition expression of SWhile.
    , _sSi         :: Stmt p    -- ^ Statement to execute while truth.
    }
  | SDo {
      _sHist       :: XSHist p -- ^ History of the statement.
    , _sXSDo       :: XSDo p   -- ^ Extra field of SDo.
    , _sExpr       :: Expr p   -- ^ Condition expression of SIf.
    , _sSi         :: Stmt p   -- ^ Statement to execute before and while truth.
    }
  | SForB {
      _sHist       :: XSHist p          -- ^ History of the statement.
    , _sXSForB     :: XSForB p          -- ^ Extra field of SForB.
    , _sForInit    :: Maybe (ForInit p) -- ^ Optional for initializer.
    , _sForECond   :: Maybe (Expr p)    -- ^ Optional condition expr of SForB.
    , _sForEPost   :: Maybe [Expr p]    -- ^ Update expressions on each iter.
    , _sSi         :: Stmt p            -- ^ Statement to execute while truth.
    }
  | SForE {
      _sHist       :: XSHist p -- ^ History of the statement.
    , _sXSForE     :: XSForE p -- ^ Extra field of SForE.
    , _sForVMTy    :: VMType p -- ^ Type of identifier in SForE.
    , _sForVar     :: Ident p  -- ^ Identifier of SForE.
    , _sExpr       :: Expr p   -- ^ The expr yielding the iteratable to iterate.
    , _sSi         :: Stmt p   -- ^ Statement to execute for each element-
    }
  | SContinue {
      _sHist       :: XSHist p     -- ^ History of the statement.
    , _sXSContinue :: XSContinue p -- ^ Extra field of SContinue.
    }
  | SBreak {
      _sHist       :: XSHist p  -- ^ History of the statement.
    , _sXSBreak    :: XSBreak p -- ^ Extra field of SBreak.
    }
  | SSwitch {
      _sHist       :: XSHist p        -- ^ History of the statement.
    , _sXSSwitch   :: XSSwitch p      -- ^ Extra field of SSwitch.
    , _sExpr       :: Expr p          -- ^ Expr to switch on.
    , _sSwiBlock   :: [SwitchBlock p] -- ^ Switch blocks to pick from.
    }

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

-- | FormalParam: formal parameter of a method.
data FormalParam p = FormalParam
  { _fpHist :: XFpHist p   -- ^ History of formal parameter.
  , _fpXFP  :: XFormPar p  -- ^ Extra field of formal parameter.
  , _fpType :: VMType p    -- ^ Type of parameter.
  , _fpVDI  :: VarDeclId p -- ^ Identifier of parameter.
  }

-- | MemberDecl: Member declarations of a class.
data MemberDecl p = MethodDecl
  { _mdHist      :: XMdHist p          -- ^ History of member declarations.
  , _mdXMethDecl :: XMethDecl p        -- ^ Extra fields of method declaration.
  , _mdRetrT     :: Maybe (XDefType p) -- ^ Optional return type / void.
  , _mdName      :: Ident p            -- ^ Name of method.
  , _mdParams    :: [FormalParam p]    -- ^ Parameters of method.
  , _mdBlock     :: Block p            -- ^ Block of method.
  }

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

-- | Declaration in a class.
data Decl p = MemberDecl 
  { _declHist :: XDeclHist p  -- ^ History of declarations.
  , _declMem  :: MemberDecl p -- ^ Member declaration of a class.
  }

-- | ClassBody: class body the class.
data ClassBody p = ClassBody
  { _cbHist  :: XCbHist p -- ^ History of ClassBody.
  , _cbDecls :: [Decl p]  -- ^ Declarations of ClassBody.
  }

-- | ClassDecl: class declaration.
data ClassDecl p = ClassDecl
  { _cdHist :: XCdHist p   -- ^ History of the ClassDecl.
  , _cdId   :: Ident p     -- ^ Identifier of the class.
  , _cdBody :: ClassBody p -- ^ Body of the class.
  }

-- | TypeDecl: type declarations in the CU.
data TypeDecl p = ClassTypeDecl
  { _tdHist  :: XTdHist p   -- ^ History of TypeDecl.
  , _tdClass :: ClassDecl p -- ^ Class declaration.
  }

-- | CompilationUnit: A whole file.
data CompilationUnit p = CompilationUnit
  { _cuHist   :: XCUHist p    -- ^ History of the CU.
  , _cuTDecls :: [TypeDecl p] -- ^ Type declarations of the CU.
  }

--------------------------------------------------------------------------------
-- Type families, History:
--------------------------------------------------------------------------------

-- | XIdHist: history type-constructor for Ident p. Indexed by phase p.
type family XIdHist   p ;

-- | XNmHist: history type-constructor for Name p. Indexed by phase p.
type family XNmHist   p ;

-- | XLVHist: history type-constructor for LValue p. Indexed by phase p.
type family XLVHist   p ;

-- | XVIHist: history type-constructor for VarInit p. Indexed by phase p.
type family XVIHist   p ;

-- | XVDIHist: history type-constructor for VarDeclId p. Indexed by phase p.
type family XVDIHist  p ;

-- | XVDHist: history type-constructor for VarDecl p. Indexed by phase p.
type family XVDHist  p ;

-- | XAIHist: history type-constructor for ArrayInit p. Indexed by phase p.
type family XAIHist   p ;

-- | XExprHist: history type-constructor for Expr p. Indexed by phase p.
type family XExprHist p ;

-- | XSHist: history type-constructor for Stmt p. Indexed by phase p.
type family XSHist    p ;

-- | XBHist: history type-constructor for Block p. Indexed by phase p.
type family XBHist    p ;

-- | XTvdHist: history type-constructor for TypedVVDecl p. Indexed by phase p.
type family XTvdHist  p ;

-- | XFiHist: history type-constructor for ForInit p. Indexed by phase p.
type family XFiHist   p ;

-- | XSbHist: history type-constructor for SwitchBlock p. Indexed by phase p.
type family XSbHist   p ;

-- | XSlHist: history type-constructor for SwitchLabel p. Indexed by phase p.
type family XSlHist   p ;

-- | XVMHist: history type-constructor for VMType p. Indexed by phase p.
type family XVMHist   p ;

-- | XMdHist: history type-constructor for MemberDecl p. Indexed by phase p.
type family XMdHist   p ;

-- | XFpHist: history type-constructor for FormalParam p. Indexed by phase p.
type family XFpHist   p ;

-- | XCUHist: history type-constructor for CompilationUnit p. Indexed by phase p.
type family XCUHist   p ;

-- | XTdHist: history type-constructor for TypeDecl p. Indexed by phase p.
type family XTdHist   p ;

-- | XCdHist: history type-constructor for ClassDecl p. Indexed by phase p.
type family XCdHist   p ;

-- | XCbHist: history type-constructor for ClassBody p. Indexed by phase p.
type family XCbHist   p ;

-- | XDeclHist: history type-constructor for Decl p. Indexed by phase p.
type family XDeclHist p ;

--------------------------------------------------------------------------------
-- Type families, Expressions, Extra fields & Constructors:
--------------------------------------------------------------------------------

-- | XELitT: type-constructor for literals in an Expr p. Indexed by phase p.
type family XELitT    p ;

-- | XDefType: type level function for explicit type definitions in an AST.
-- Indexed by phase p.
type family XDefType  p ;

type family XLVName   p ; type family XLVArr    p ;

type family XExpr     p ; type family XELit     p ; type family XEVar     p ;
type family XECast    p ; type family XECond    p ; type family XEAssign  p ;
type family XEOAssign p ; type family XENum     p ; type family XECmp     p ;
type family XELog     p ; type family XENot     p ; type family XEStep    p ;
type family XEBCompl  p ; type family XEPlus    p ; type family XEMinus   p ;
type family XEMApp    p ; type family XEArrNew  p ; type family XESysOut  p ;

--------------------------------------------------------------------------------
-- Type families, Statements, Extra fields & Constructors:
--------------------------------------------------------------------------------

type family XStmt     p ; type family XSEmpty    p ; type family XSBlock  p ;
type family XSExpr    p ; type family XSVars     p ; type family XSReturn p ;
type family XSVReturn p ; type family XSIf       p ; type family XSIfElse p ;
type family XSWhile   p ; type family XSDo       p ; type family XSForB   p ;
type family XSForE    p ; type family XSContinue p ; type family XSBreak  p ;
type family XSSwitch  p ;
type family XBlock    p ; type family XMethDecl  p ; type family XFormPar p ;

--------------------------------------------------------------------------------
-- Constraint constructor for type families:
--------------------------------------------------------------------------------

-- | ForallXHist: A constraint-constructor for type families regarding history.
-- ForallXHist :: (* -> Constraint) -> * -> Constraint
type ForallXHist (fi :: * -> Constraint) p
  = (
  -- History:
    fi (XIdHist    p)
  , fi (XNmHist    p)
  , fi (XLVHist    p)
  , fi (XVIHist    p)
  , fi (XVDIHist   p)
  , fi (XVDHist    p)
  , fi (XAIHist    p)
  , fi (XExprHist  p)
  , fi (XVMHist    p)
  , fi (XBHist     p)
  , fi (XSHist     p)
  , fi (XSlHist    p)
  , fi (XTvdHist   p)
  , fi (XFiHist    p)
  , fi (XSbHist    p)
  , fi (XMdHist    p)
  , fi (XFpHist    p)
  , fi (XCUHist    p)
  , fi (XTdHist    p)
  , fi (XCdHist    p)
  , fi (XCbHist    p)
  , fi (XDeclHist  p)
  )

-- | ForallXExpr: A constraint-constructor for type families for Expr type.
-- ForallXExpr :: (* -> Constraint) -> * -> Constraint
type ForallXExpr (fi :: * -> Constraint) p
  = (
  -- LValues, constructors & fields:
    fi (XLVName    p)
  , fi (XLVArr     p)
  -- Expressions, constructors & fields:
  , fi (XExpr      p)
  , fi (XELit      p)
  , fi (XEVar      p)
  , fi (XECast     p)
  , fi (XECond     p)
  , fi (XEAssign   p)
  , fi (XEOAssign  p)
  , fi (XENum      p)
  , fi (XECmp      p)
  , fi (XELog      p)
  , fi (XENot      p)
  , fi (XEStep     p)
  , fi (XEBCompl   p)
  , fi (XEPlus     p)
  , fi (XEMinus    p)
  , fi (XEMApp     p)
  , fi (XEArrNew   p)
  , fi (XESysOut   p)
  )

-- | ForallXStmt: A constraint-constructor for type families for Stmt type.
-- ForallXStmt :: (* -> Constraint) -> * -> Constraint
type ForallXStmt (fi :: * -> Constraint) p
  = (
  -- Expressions, constructors & fields:
    fi (XSEmpty    p)
  , fi (XStmt      p)
  , fi (XSEmpty    p)
  , fi (XSBlock    p)
  , fi (XSExpr     p)
  , fi (XSVars     p)
  , fi (XSReturn   p)
  , fi (XSVReturn  p)
  , fi (XSIf       p)
  , fi (XSIfElse   p)
  , fi (XSWhile    p)
  , fi (XSDo       p)
  , fi (XSForB     p)
  , fi (XSForE     p)
  , fi (XSContinue p)
  , fi (XSBreak    p)
  , fi (XSSwitch   p)
  )

-- | ForallXGen: A constraint-constructor for type families for general stuff.
-- ForallXGen :: (* -> Constraint) -> * -> Constraint
type ForallXGen (fi :: * -> Constraint) p
  = (
  -- General:
    fi (XDefType   p)
  , fi (XELitT     p)
  , fi (XMethDecl  p)
  , fi (XFormPar   p)
  )

-- | ForallX: A constraint-constructor for all type families used by any nodes.
-- ForallX :: (* -> Constraint) -> * -> Constraint
type ForallX (fi :: * -> Constraint) p
  = (
    fi p
  , ForallXGen  fi p
  , ForallXHist fi p
  , ForallXExpr fi p
  , ForallXStmt fi p
  , fi (XBlock     p)
  )

--------------------------------------------------------------------------------
-- Approx instances:
--------------------------------------------------------------------------------

instance Approx a => Approx [a] where
  a ~~ b = all (uncurry (~~)) $ zip a b

instance Approx a => Approx (Maybe a) where
  Just a  ~~ Just b  = True
  Nothing ~~ Nothing = True
  _       ~~ _       = False

instance Approx (Ident  p) where (~~) = (==) `on` _idId
instance Approx (Name   p) where (~~) = (~~) `on` _nmIds
instance Approx NumOp      where (~~) = (==)
instance Approx CmpOp      where (~~) = (==)
instance Approx LogOp      where (~~) = (==)
instance Approx StepOp     where (~~) = (==)
instance Approx Integer    where (~~) = (==)
instance Approx VarMod     where (~~) = (==)

instance ForallX Approx p => Approx (LValue p) where
  LVName  _ la lb    ~~ LVName  _ ra rb    = la ~~ ra && lb ~~ rb
  LVArray _ la lb lc ~~ LVArray _ ra rb rc = la ~~ ra && lb ~~ rb && lc ~~ rc
  _                  ~~ _                  = False

instance ForallX Approx p => Approx (Expr p) where
  EExpr _ la          ~~ EExpr _ ra          = la ~~ ra
  ELit  _ la lb       ~~ ELit _ ra rb        = la ~~ ra && lb ~~ rb
  EVar  _ la lb       ~~ EVar _ ra rb        = la ~~ ra && lb ~~ rb
  ECast _ la lb lc    ~~ ECast _ ra rb rc    = la ~~ ra && lb ~~ rb && lc ~~ rc
  ECond _ la lb lc ld ~~ ECond _ ra rb rc rd =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  EAssign _ la lb lc  ~~ EAssign _ ra rb rc  =
    la ~~ ra && lb ~~ rb && lc ~~ rc
  EOAssign _ la lb lc ld ~~ EOAssign _ ra rb rc rd =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  ENum _ la lb lc ld  ~~ ENum _ ra rb rc rd  =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  ECmp _ la lb lc ld  ~~ ECmp _ ra rb rc rd  =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  ELog _ la lb lc ld  ~~ ELog _ ra rb rc rd  =
    la ~~ ra && lb   ~~ rb && lc ~~ rc && ld ~~ rd
  ENot    _ la lb     ~~ ENot    _ ra rb     = la ~~ ra && lb ~~ rb
  EStep   _ la lb lc  ~~ EStep   _ ra rb rc  = la ~~ ra && lb ~~ rb && lc ~~ rc
  EBCompl _ la lb     ~~ EBCompl _ ra rb     = la ~~ ra && lb ~~ rb
  EPlus   _ la lb     ~~ EPlus   _ ra rb     = la ~~ ra && lb ~~ rb
  EMinus  _ la lb     ~~ EMinus  _ ra rb     = la ~~ ra && lb ~~ rb
  EMApp   _ la lb lc  ~~ EMApp   _ ra rb rc  = la ~~ ra && lb ~~ rb && lc ~~ rc
  EArrNew _ la lb lc ld  ~~ EArrNew _ ra rb rc rd =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  EArrNewI _ la lb lc ld ~~ EArrNewI _ ra rb rc rd =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  ESysOut _ la lb        ~~ ESysOut _ ra rb  = la ~~ ra && lb ~~ rb
  _                   ~~ _                   = False

instance ForallX Approx p => Approx (VarInit p) where
  InitExpr _ la ~~ InitExpr _ ra = la ~~ ra
  InitArr  _ la ~~ InitArr  _ ra = la ~~ ra
  _             ~~ _             = False

instance ForallX Approx p => Approx (ArrayInit p) where
  (~~) = (~~) `on` _aiVIs

instance ForallX Approx p => Approx (VarDeclId p) where
  VarDId  _ la    ~~ VarDId  _ ra    = la ~~ ra
  VarDArr _ la lb ~~ VarDArr _ ra rb = la ~~ ra && lb ~~ rb
  _               ~~ _               = False

instance ForallX Approx p => Approx (VarDecl p) where
  VarDecl _ la lb ~~ VarDecl _ ra rb = la ~~ ra && rb ~~ rb

instance ForallX Approx p => Approx (VMType p) where
  VMType _ la lb ~~ VMType _ ra rb  = la ~~ ra && rb ~~ rb

instance ForallX Approx p => Approx (TypedVVDecl p) where
  TypedVVDecl _ la lb ~~ TypedVVDecl _ ra rb  = la ~~ ra && rb ~~ rb

instance ForallX Approx p => Approx (ForInit p) where
  FIVars  _ la ~~ FIVars  _ ra = la ~~ ra
  FIExprs _ la ~~ FIExprs _ ra = la ~~ ra
  _            ~~ _            = False

instance ForallX Approx p => Approx (SwitchBlock p) where
  SwitchBlock _ la lb ~~ SwitchBlock _ ra rb = la ~~ ra && lb ~~ rb

instance ForallX Approx p => Approx (SwitchLabel p) where
  SwitchCase _ la ~~ SwitchCase _ ra = la ~~ ra
  Default    _    ~~ Default    _    = True
  _               ~~ _               = False

instance ForallX Approx p => Approx (Block p) where
  Block _ la lb ~~ Block _ ra rb = la ~~ ra && lb ~~ rb

instance ForallX Approx p => Approx (Stmt p) where
  SStmt    _ la          ~~ SStmt    _ ra     = la ~~ ra
  SEmpty   _ la          ~~ SEmpty   _ ra     = la ~~ ra
  SBlock   _ la lb       ~~ SBlock   _ ra rb  = la ~~ ra && lb ~~ rb
  SExpr    _ la lb       ~~ SExpr    _ ra rb  = la ~~ ra && lb ~~ rb
  SVars    _ la lb       ~~ SVars    _ ra rb  = la ~~ ra && lb ~~ rb
  SReturn  _ la lb       ~~ SReturn  _ ra rb  = la ~~ ra && lb ~~ rb
  SVReturn _ la          ~~ SVReturn _ ra     = la ~~ ra
  SIf      _ la lb lc    ~~ SIf _ ra rb rc    = la ~~ ra && lb ~~ rb && lc ~~ rc
  SIfElse  _ la lb lc ld ~~ SIfElse _ ra rb rc rd =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd
  SWhile   _ la lb lc   ~~ SWhile _ ra rb rc  = la ~~ ra && lb ~~ rb && lc ~~ rc
  SDo      _ la lb lc   ~~ SDo _ ra rb rc     = la ~~ ra && lb ~~ rb && lc ~~ rc
  SForB    _ la lb lc ld le ~~ SForB _ ra rb rc rd re =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd && le ~~ re
  SForE   _ la lb lc ld le ~~ SForE _ ra rb rc rd re =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd && le ~~ re
  SContinue _ la        ~~ SContinue  _ ra    = la ~~ ra
  SBreak    _ la        ~~ SBreak     _ ra    = la ~~ ra
  SSwitch   _ la lb lc  ~~ SSwitch _ ra rb rc = la ~~ ra && lb ~~ rb && lc ~~ rc
  _                     ~~ _                  = False

instance ForallX Approx p => Approx (FormalParam p) where
  FormalParam _ la lb lc ~~ FormalParam _ ra rb rc =
    la ~~ ra && lb ~~ rb && lc ~~ rc

instance ForallX Approx p => Approx (MemberDecl p) where
  MethodDecl _ la lb lc ld le ~~ MethodDecl _ ra rb rc rd re =
    la ~~ ra && lb ~~ rb && lc ~~ rc && ld ~~ rd && le ~~ re

instance ForallX Approx p => Approx (Decl p) where
  MemberDecl _ la ~~ MemberDecl _ ra = la ~~ ra

instance ForallX Approx p => Approx (ClassBody p) where
  ClassBody _ la ~~ ClassBody _ ra = la ~~ ra

instance ForallX Approx p => Approx (ClassDecl p) where
  ClassDecl _ la lb ~~ ClassDecl _ ra rb = la ~~ ra && lb ~~ rb

instance ForallX Approx p => Approx (TypeDecl p) where
  ClassTypeDecl _ la ~~ ClassTypeDecl _ ra = la ~~ ra

instance ForallX Approx p => Approx (CompilationUnit p) where
  CompilationUnit _ la ~~ CompilationUnit _ ra = la ~~ ra

--------------------------------------------------------------------------------
-- Other instances:
--------------------------------------------------------------------------------

$(let clazz = [''Eq, ''Ord, ''Show, ''Read, ''Typeable, ''Data, ''Generic]
      types = [ ''Ident, ''Name, ''LValue, ''VarInit, ''ArrayInit, ''Expr
              , ''VarDeclId, ''VMType, ''VarDecl, ''TypedVVDecl, ''Block
              , ''ForInit, ''SwitchLabel, ''SwitchBlock, ''Stmt, ''MemberDecl
              , ''FormalParam, ''CompilationUnit, ''TypeDecl, ''ClassDecl
              , ''ClassBody, ''Decl]
  in  (++) <$> stdDerives ''ForallX clazz types <*> deriveLens types
  )