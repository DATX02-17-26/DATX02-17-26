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
    DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveAnyClass
  , TemplateHaskell, LambdaCase
  #-}

-- | Annotated AST.
module CoreS.Annotated.AST where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Util.TH (deriveLens)

import CoreS.Common.Common

--------------------------------------------------------------------------------
-- lvalues:
--------------------------------------------------------------------------------

-- | LValue: An LValue, as opposed to RValue, is an "expression" that can
-- occur on the left hand side of an assignment. I.e: it models what it means
-- to be a location in memory.
data LValue a
  = LVName {
      _lvAnot  :: a        -- ^ Annotation
    , _lvId    :: Name     -- ^ A variable / property / static field identifier.
    }
  | LVArray {
      _lvAnot  :: a        -- ^ Annotation
    , _lvExpr  :: Expr a   -- ^ An expression yielding an array.
    , _lvExprs :: [Expr a] -- ^ Indexing expressions, must be castable to int.
    }
  | HoleLValue {
      _lvHole  :: Hole     -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

--------------------------------------------------------------------------------
-- Variable & Array initialization:
--------------------------------------------------------------------------------

-- | VarInit: initializer of variables.
data VarInit a
  = InitExpr {
      _viAnot    :: a
    , _viExpr    :: Expr a      -- ^ Initializer expression.
    }
  | InitArr {
      _viAnot    :: a
    , _viArrInit :: ArrayInit a -- ^ Initializing an array.
    }
  | HoleVarInit {
      _viHole    :: Hole        -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | ArrayInit: Initializer of array creation (new T[]...) expressions.
data ArrayInit a = ArrayInit
  {
    _aiAnot :: a
  , _aiVIs  :: [VarInit a] -- ^ List of initializers for each element in array.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

--------------------------------------------------------------------------------
-- Expressions:
--------------------------------------------------------------------------------

-- | Expr: the type of expressions, phase-indexed by p.
data Expr a
  = ELit {
      _eAnot     :: a
    , _eLit      :: Literal     -- ^ Literal of an ELit.
    }
  | EVar  {
      _eAnot     :: a
    , _eLValue   :: LValue a    -- ^ LValue of a variable reference.
    }
  | ECast {
      _eAnot     :: a
    , _eDefT     :: Type        -- ^ To-cast type of an ECast, indexed by p.
    , _eExpr     :: Expr a      -- ^ Expr to cast to other type.
    }
  | ECond {
      _eAnot     :: a
    , _eCond     :: Expr a      -- ^ Condition expr of a ternary expr.
    , _eExpI     :: Expr a      -- ^ Expr to eval on truth of condition expr.
    , _eExpE     :: Expr a      -- ^ Expr to eval on falsehood of condition expr.
    }
  | EAssign {
      _eAnot     :: a
    , _eLValue   :: LValue a    -- ^ Variable reference to assign to.
    , _eExpr     :: Expr a      -- ^ Expression to assign to variable reference.
    }
  | EOAssign {
      _eAnot     :: a
    , _eLValue   :: LValue a    -- ^ Variable reference to assign to.
    , _eNumOp    :: NumOp       -- ^ Assignment operator.
    , _eExpr     :: Expr a      -- ^ Expression to modify variable ref with.
    }
  | ENum {
      _eAnot     :: a
    , _eNumOp    :: NumOp       -- ^ Numeric operation to compute.
    , _eLeft     :: Expr a      -- ^ Left operand.
    , _eRight    :: Expr a      -- ^ Right operand.
    }
  | ECmp {
      _eAnot     :: a
    , _eCmpOp    :: CmpOp       -- ^ Comparison operator to compute.
    , _eLeft     :: Expr a      -- ^ Left operand.
    , _eRight    :: Expr a      -- ^ Right operand.
    }
  | ELog {
      _eAnot     :: a
    , _eLogOp    :: LogOp       -- ^ Logical operator to compute.
    , _eLeft     :: Expr a      -- ^ Left operand.
    , _eRight    :: Expr a      -- ^ Right operand.
    }
  | ENot {
      _eAnot     :: a
    , _eExpr     :: Expr a      -- ^ Expression to compute logical not on.
    }
  | EStep {
      _eAnot     :: a
    , _eStepOp   :: StepOp      -- ^ Stepping operator.
    , _eExpr     :: Expr a      -- ^ Expression to step, must be variable ref.
    }
  | EBCompl {
      _eAnot     :: a
    , _eExpr     :: Expr a      -- ^ Expr to compute bitwise complement on.
    }
  | EPlus {
      _eAnot     :: a
    , _eExpr     :: Expr a      -- ^ Expr to compute unary plus on.
    }
  | EMinus {
      _eAnot     :: a
    , _eExpr     :: Expr a      -- ^ Expr to compute unary negation of.
    }
  | EMApp {
      _eAnot     :: a
    , _eName     :: Name        -- ^ Name of method to apply.
    , _eExprs    :: [Expr a]
    }
  | EInstNew {                  -- Constructs a new object of _eName type.
      _eAnot     :: a
    , _eName     :: Name        -- ^ Name of the type to be instantiated.
    , _eExprs    :: [Expr a]    -- ^ Zero or more exprs to pass to constructor.
    }
  | EArrNew {
      _eAnot     :: a
    , _eDefT     :: Type        -- ^ Base-type of array to construct.
    , _eExprs    :: [Expr a]    -- ^ Expressions to compute lengths with.
    , _eEDims    :: Integer     -- ^ Number of extra dimensions.
    }
  | EArrNewI {
      _eAnot     :: a
    , _eDefT     :: Type        -- ^ Base-type of array to construct.
    , _eDims     :: Integer     -- ^ Number of dimensions.
    , _eAInit    :: ArrayInit a -- ^ Array initializer.
    }
  | ESysOut {
      _eAnot     :: a
    , _eExpr     :: Expr a      -- ^ Expression to print out.
    }
  | HoleExpr {
      _eHole     :: Hole        -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

-- | VarDecl: variable declaration (id + initializer).
data VarDecl a
  = VarDecl {
      _vdAnot  :: a
    , _vdVDI   :: VarDeclId         -- ^ Identifier part of variable.
    , _vdVInit :: Maybe (VarInit a) -- ^ Potential variable initializer.
    }
  | HoleVarDecl {
      _vdHole :: Hole               -- ^ TODO DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | TypedVVDecl: Typed Variable declarations.
data TypedVVDecl a
  = TypedVVDecl {
      _tvdAnot   :: a
    , _tvdType   :: VMType      -- ^ Type of variable declarations.
    , _tvdVDecls :: [VarDecl a] -- ^ List of variable declarations.
    }
  | HoleTypedVVDecl {
      _tvdHole   :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | ForInit: For loop initializer for normal for loops.
data ForInit a
  = FIVars {
      _fiAnot  :: a
    , _fiVars  :: TypedVVDecl a -- ^ For loop variable declarations.
    }
  | FIExprs {
      _fiAnot  :: a
    , _fiExprs :: [Expr a]      -- ^ For loop initial variable expressions.
    }
  | HoleForInit {
      _fiHole  ::  Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | SwitchLabel (case).
data SwitchLabel a
  = SwitchCase {
      _slAnot :: a
    , _slExpr :: Expr a -- ^ Case expression.
    }
  | Default             -- ^ Default case.
  | HoleSwitchLabel {
      _slHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | SwitchBlock: One match arm of a switch block.
data SwitchBlock a
  = SwitchBlock {
      _sbAnot  :: a
    , _sbLab   :: SwitchLabel a -- ^ SwitchLabel of the block.
    , _sbBlock :: Block a       -- ^ Block of the SwitchBlock.
    }
  | HoleSwitchBlock {
      _sbHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | Block of statements.
data Block a
  = Block {
      _bAnot  :: a
    , _bStmts :: [Stmt a] -- ^ List of statements in the block.
    }
  | HoleBlock {
      _bHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | Stmt: A statement, unlike expressions, it has no value.
data Stmt a
  = SEmpty {
      _sAnot       :: a
    }
  | SBlock {
      _sAnot       :: a
    , _sBlock      :: Block a  -- ^ The block of the SBlock.
    }
  | SExpr {
      _sAnot       :: a
    , _sExpr       :: Expr a   -- ^ The Expr of the SExpr.
    }
  | SVars {
      _sAnot       :: a
    , _sVDecl      :: TypedVVDecl a -- ^ The variable declarations of SVars.
    }
  | SReturn {
      _sAnot       :: a
    , _sExpr       :: Expr a  -- ^ The Expression to return.
    }
  | SVReturn {
      _sAnot       :: a
    }
  | SIf {
      _sAnot       :: a
    , _sExpr       :: Expr a  -- ^ Condition expression of SIf.
    , _sSi         :: Stmt a  -- ^ Statement to execute on truth.
    }
  | SIfElse {
      _sAnot       :: a
    , _sExpr       :: Expr a  -- ^ Condition expression of SIf.
    , _sSi         :: Stmt a  -- ^ Statement to execute on truth.
    , _sSe         :: Stmt a  -- ^ Statement to execute on falsehood.
    }
  | SWhile {
      _sAnot       :: a
    , _sExpr       :: Expr a  -- ^ Condition expression of SWhile.
    , _sSi         :: Stmt a  -- ^ Statement to execute while truth.
    }
  | SDo {
      _sAnot       :: a
    , _sExpr       :: Expr a  -- ^ Condition expression of SIf.
    , _sSi         :: Stmt a  -- ^ Statement to execute before and while truth.
    }
  | SForB {
      _sAnot       :: a
    , _sForInit    :: Maybe (ForInit a) -- ^ Optional for initializer.
    , _sForECond   :: Maybe (Expr a)    -- ^ Optional condition expr of SForB.
    , _sForEPost   :: Maybe [Expr a]    -- ^ Update expressions on each iter.
    , _sSi         :: Stmt a            -- ^ Statement to execute while truth.
    }
  | SForE {
      _sAnot       :: a
    , _sForVMTy    :: VMType -- ^ Type of identifier in SForE.
    , _sForVar     :: Ident  -- ^ Identifier of SForE.
    , _sExpr       :: Expr a -- ^ The expr yielding the iteratable to iterate.
    , _sSi         :: Stmt a -- ^ Statement to execute for each element.
    }
  | SContinue {
      _sAnot       :: a
    }
  | SBreak {
      _sAnot       :: a
    }
  | SSwitch {
      _sAnot       :: a
    , _sExpr       :: Expr a          -- ^ Expr to switch on.
    , _sSwiBlock   :: [SwitchBlock a] -- ^ Switch blocks to pick from.
    }
  | HoleStmt {
      _sHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

-- | MemberDecl: Member declarations of a class.
data MemberDecl a
  = MethodDecl {
      _mdAnot      :: a
    , _mdRetrT     :: RType         -- ^ Optional return type / void.
    , _mdName      :: Ident         -- ^ Name of method.
    , _mdParams    :: [FormalParam] -- ^ Parameters of method.
    , _mdBlock     :: Block a       -- ^ Block of method.
    }
  | HoleMemberDecl {
      _mdHole      :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

-- | Declaration in a class.
data Decl a
  = MemberDecl {
      _declAnot :: a
    , _declMem  :: MemberDecl a -- ^ Member declaration of a class.
    }
  | HoleDecl {
      _decHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | ClassBody: class body the class.
data ClassBody a
  = ClassBody {
      _cbAnot  :: a
    , _cbDecls :: [Decl a]  -- ^ Declarations of ClassBody.
    }
  | HoleClassBody {
      _cbHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | ClassDecl: class declaration.
data ClassDecl a
  = ClassDecl {
      _cdAnot :: a
    , _cdId   :: Ident       -- ^ Identifier of the class.
    , _cdBody :: ClassBody a -- ^ Body of the class.
    }
  | HoleClassDecl {
      _cdHole :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | TypeDecl: type declarations in the CU.
data TypeDecl a
  = ClassTypeDecl {
      _tdAnot  :: a
    , _tdClass :: ClassDecl a -- ^ Class declaration.
    }
  | HoleTypeDecl {
      _tdHole  :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | An Import declaration allowing things to be referred to by unqualified
-- identifiers.
--
-- If _idStatic is True, the object referred to is a static member,
-- which is either a static field or a static method. Otherwise,
-- it is a type.
--
-- If _idWild is True, then all things (depending on _idStatic) in the given
-- package or class in _idName are imported.
data ImportDecl a
  = ImportDecl {
      _idAnot   :: a
    , _idName   :: Name -- ^ Name to import.
    , _idStatic :: Bool -- ^ Import static member?
    , _idWild   :: Bool -- ^ Import with wildcard? I.e: .*
    }
  | HoleImportDecl {
      _idHole   :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

-- | CompilationUnit: A whole file.
data CompilationUnit a
  = CompilationUnit {
      _cuAnot    :: a
    , _cuImports :: [ImportDecl a] -- ^ Import declarations.
    , _cuTDecls  :: [TypeDecl a]   -- ^ Type declarations of the CU.
    }
  | HoleCompilationUnit {
      _cuHole   :: Hole
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, NFData, Functor)

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [ ''Literal, ''LValue, ''VarInit, ''ArrayInit, ''Expr
             , ''VarDecl, ''TypedVVDecl, ''Block
             , ''ForInit, ''SwitchLabel, ''SwitchBlock, ''Stmt, ''MemberDecl
             , ''CompilationUnit, ''TypeDecl, ''ClassDecl
             , ''ClassBody, ''Decl, ''ImportDecl
             ])