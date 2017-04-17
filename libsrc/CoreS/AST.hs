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

{-# LANGUAGE
    DeriveDataTypeable, DeriveGeneric, DeriveAnyClass
  , TemplateHaskell, LambdaCase
  #-}

-- | Main AST.
module CoreS.AST (
    module RE
  , module CoreS.AST
  ) where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Util.TH (deriveLens)

import CoreS.Common.Common as RE

--------------------------------------------------------------------------------
-- lvalues:
--------------------------------------------------------------------------------

-- | LValue: An LValue, as opposed to RValue, is an "expression" that can
-- occur on the left hand side of an assignment. I.e: it models what it means
-- to be a location in memory.
data LValue
  = LVName {
      _lvId    :: Name   -- ^ A variable / property / static field identifier.
    }
  | LVArray {
      _lvExpr  :: Expr   -- ^ An expression yielding an array.
    , _lvExprs :: [Expr] -- ^ Indexing expressions, must be castable to int.
    }
  | HoleLValue {
      _lvHole  :: Hole   -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Variable & Array initialization:
--------------------------------------------------------------------------------

-- | VarInit: initializer of variables.
data VarInit
  = InitExpr {
      _viExpr    :: Expr      -- ^ Initializer expression.
    }
  | InitArr {
      _viArrInit :: ArrayInit -- ^ Initializing an array.
    }
  | HoleVarInit {
      _viHole    :: Hole      -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | ArrayInit: Initializer of array creation (new T[]...) expressions.
data ArrayInit = ArrayInit
  {
    _aiVIs  :: [VarInit] -- ^ List of initializers for each element in array.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Expressions:
--------------------------------------------------------------------------------

-- | Expr: the type of expressions, phase-indexed by p.
data Expr
  = ELit {
      _eLit      :: Literal     -- ^ Literal of an ELit.
  }
  | EVar  {
      _eLValue   :: LValue      -- ^ LValue of a variable reference.
  }
  | ECast {
      _eDefT     :: Type        -- ^ To-cast type of an ECast, indexed by p.
    , _eExpr     :: Expr        -- ^ Expr to cast to other type.
    }
  | ECond {
      _eCond     :: Expr        -- ^ Condition expr of a ternary expr.
    , _eExpI     :: Expr        -- ^ Expr to eval on truth of condition expr.
    , _eExpE     :: Expr        -- ^ Expr to eval on falsehood of condition expr.
  }
  | EAssign {
      _eLValue   :: LValue      -- ^ Variable reference to assign to.
    , _eExpr     :: Expr        -- ^ Expression to assign to variable reference.
  }
  | EOAssign {
      _eLValue   :: LValue      -- ^ Variable reference to assign to.
    , _eNumOp    :: NumOp       -- ^ Assignment operator.
    , _eExpr     :: Expr        -- ^ Expression to modify variable ref with.
    }
  | ENum {
      _eNumOp    :: NumOp       -- ^ Numeric operation to compute.
    , _eLeft     :: Expr        -- ^ Left operand.
    , _eRight    :: Expr        -- ^ Right operand.
  }
  | ECmp {
      _eCmpOp    :: CmpOp       -- ^ Comparison operator to compute.
    , _eLeft     :: Expr        -- ^ Left operand.
    , _eRight    :: Expr        -- ^ Right operand.
  }
  | ELog {
      _eLogOp    :: LogOp       -- ^ Logical operator to compute.
    , _eLeft     :: Expr        -- ^ Left operand.
    , _eRight    :: Expr        -- ^ Right operand.
  }
  | ENot {
      _eExpr     :: Expr        -- ^ Expression to compute logical not on.
    }
  | EStep {
      _eStepOp   :: StepOp      -- ^ Stepping operator.
    , _eExpr     :: Expr        -- ^ Expression to step, must be variable ref.
    }
  | EBCompl {
      _eExpr     :: Expr        -- ^ Expr to compute bitwise complement on.
    }
  | EPlus {
      _eExpr     :: Expr        -- ^ Expr to compute unary plus on.
    }
  | EMinus {
      _eExpr     :: Expr        -- ^ Expr to compute unary negation of.
    }
  | EMApp {
      _eName     :: Name        -- ^ Name of method to apply.
    , _eExprs    :: [Expr]
    }
  | EInstNew {                  -- Constructs a new object of _eName type.
      _eName     :: Name        -- ^ Name of the type to be instantiated.
    , _eExprs    :: [Expr]      -- ^ Zero or more exprs to pass to constructor.
    }
  | EArrNew {
      _eDefT     :: Type        -- ^ Base-type of array to construct.
    , _eExprs    :: [Expr]      -- ^ Expressions to compute lengths with.
    , _eEDims    :: Integer     -- ^ Number of extra dimensions.
    }
  | EArrNewI {
      _eDefT     :: Type        -- ^ Base-type of array to construct.
    , _eDims     :: Integer     -- ^ Number of dimensions.
    , _eAInit    :: ArrayInit   -- ^ Array initializer.
    }
  | ESysOut {
      _eExpr     :: Expr        -- ^ Expression to print out.
    }
  | HoleExpr {
      _eHole     :: Hole        -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

-- | VarDecl: variable declaration (id + initializer).
data VarDecl
  = VarDecl {
      _vdVDI   :: VarDeclId     -- ^ Identifier part of variable.
    , _vdVInit :: Maybe VarInit -- ^ Potential variable initializer.
    }
  | HoleVarDecl {
      _vdHole :: Hole           -- ^ TODO DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | TypedVVDecl: Typed Variable declarations.
data TypedVVDecl
  = TypedVVDecl {
      _tvdType   :: VMType    -- ^ Type of variable declarations.
    , _tvdVDecls :: [VarDecl] -- ^ List of variable declarations.
    }
  | HoleTypedVVDecl {
      _tvdHole   :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | ForInit: For loop initializer for normal for loops.
data ForInit
  = FIVars {
      _fiVars  :: TypedVVDecl -- ^ For loop variable declarations.
    }
  | FIExprs {
      _fiExprs :: [Expr]      -- ^ For loop initial variable expressions.
    }
  | HoleForInit {
      _fiHole  ::  Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | SwitchLabel (case).
data SwitchLabel
  = SwitchCase {
      _slExpr :: Expr -- ^ Case expression.
    }
  | Default           -- ^ Default case.
  | HoleSwitchLabel {
      _slHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | SwitchBlock: One match arm of a switch block.
data SwitchBlock
  = SwitchBlock {
      _sbLab   :: SwitchLabel -- ^ SwitchLabel of the block.
    , _sbBlock :: Block       -- ^ Block of the SwitchBlock.
    }
  | HoleSwitchBlock {
      _sbHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | Block of statements.
data Block
  = Block {
      _bStmts :: [Stmt] -- ^ List of statements in the block.
    }
  | HoleBlock {
      _bHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | Stmt: A statement, unlike expressions, it has no value.
data Stmt
  = SEmpty
  | SBlock {
      _sBlock      :: Block  -- ^ The block of the SBlock.
    }
  | SExpr {
      _sExpr       :: Expr   -- ^ The Expr of the SExpr.
    }
  | SVars {
      _sVDecl      :: TypedVVDecl -- ^ The variable declarations of SVars.
    }
  | SReturn {
      _sExpr       :: Expr   -- ^ The Expression to return.
    }
  | SVReturn
  | SIf {
      _sExpr       :: Expr   -- ^ Condition expression of SIf.
    , _sSi         :: Stmt   -- ^ Statement to execute on truth.
    }
  | SIfElse {
      _sExpr       :: Expr   -- ^ Condition expression of SIf.
    , _sSi         :: Stmt   -- ^ Statement to execute on truth.
    , _sSe         :: Stmt   -- ^ Statement to execute on falsehood.
    }
  | SWhile {
      _sExpr       :: Expr   -- ^ Condition expression of SWhile.
    , _sSi         :: Stmt   -- ^ Statement to execute while truth.
    }
  | SDo {
      _sExpr       :: Expr   -- ^ Condition expression of SIf.
    , _sSi         :: Stmt   -- ^ Statement to execute before and while truth.
    }
  | SForB {
      _sForInit    :: Maybe ForInit -- ^ Optional for initializer.
    , _sForECond   :: Maybe Expr    -- ^ Optional condition expr of SForB.
    , _sForEPost   :: Maybe [Expr]  -- ^ Update expressions on each iter.
    , _sSi         :: Stmt          -- ^ Statement to execute while truth.
    }
  | SForE {
      _sForVMTy    :: VMType -- ^ Type of identifier in SForE.
    , _sForVar     :: Ident  -- ^ Identifier of SForE.
    , _sExpr       :: Expr   -- ^ The expr yielding the iteratable to iterate.
    , _sSi         :: Stmt   -- ^ Statement to execute for each element.
    }
  | SContinue
  | SBreak
  | SSwitch {
      _sExpr       :: Expr          -- ^ Expr to switch on.
    , _sSwiBlock   :: [SwitchBlock] -- ^ Switch blocks to pick from.
    }
  | HoleStmt {
      _sHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

-- | MemberDecl: Member declarations of a class.
data MemberDecl
  = MethodDecl {
      _mdRetrT     :: RType         -- ^ Optional return type / void.
    , _mdName      :: Ident         -- ^ Name of method.
    , _mdParams    :: [FormalParam] -- ^ Parameters of method.
    , _mdBlock     :: Block         -- ^ Block of method.
    }
  | HoleMemberDecl {
      _mdHole      :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

data MethodBody = MethodBody Block | HoleMethodBody Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

-- | Declaration in a class.
data Decl
  = MemberDecl {
      _declMem  :: MemberDecl -- ^ Member declaration of a class.
    }
  | HoleDecl {
      _decHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | ClassBody: class body the class.
data ClassBody
  = ClassBody {
      _cbDecls :: [Decl]  -- ^ Declarations of ClassBody.
    }
  | HoleClassBody {
      _cbHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | ClassDecl: class declaration.
data ClassDecl
  = ClassDecl {
      _cdId   :: Ident       -- ^ Identifier of the class.
    , _cdBody :: ClassBody   -- ^ Body of the class.
    }
  | HoleClassDecl {
      _cdHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | TypeDecl: type declarations in the CU.
data TypeDecl
  = ClassTypeDecl {
    _tdClass :: ClassDecl -- ^ Class declaration.
  }
  | HoleTypeDecl {
    _tdHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | An Import declaration allowing things to be referred to by unqualified
-- identifiers.
--
-- If _idStatic is True, the object referred to is a static member,
-- which is either a static field or a static method. Otherwise,
-- it is a type.
--
-- If _idWild is True, then all things (depending on _idStatic) in the given
-- package or class in _idName are imported.
data ImportDecl
  = ImportDecl {
      _idName   :: Name -- ^ Name to import.
    , _idStatic :: Bool -- ^ Import static member?
    , _idWild   :: Bool -- ^ Import with wildcard? I.e: .*
    }
  | HoleImportDecl {
      _idHole   :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

-- | CompilationUnit: A whole file.
data CompilationUnit
  = CompilationUnit {
      _cuImports :: [ImportDecl] -- ^ Import declarations.
    , _cuTDecls  :: [TypeDecl]   -- ^ Type declarations of the CU.
    }
  | HoleCompilationUnit {
      _cuHole   :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [ ''LValue, ''VarInit, ''ArrayInit, ''Expr
             , ''VarDecl, ''TypedVVDecl, ''Block
             , ''ForInit, ''SwitchLabel, ''SwitchBlock, ''Stmt, ''MemberDecl
             , ''CompilationUnit, ''TypeDecl, ''ClassDecl
             , ''ClassBody, ''Decl, ''ImportDecl
             ])