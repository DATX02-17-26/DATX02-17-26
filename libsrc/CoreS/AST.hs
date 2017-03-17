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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, LambdaCase #-}

module CoreS.AST where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Control.Lens ((^?), isn't)

import Class.Sizeables
import Util.TH

--------------------------------------------------------------------------------
-- Names and identifiers:
--------------------------------------------------------------------------------

-- | Ident: Identifier of a variable, class, method, etc.
data Ident = Ident
  { _idId   :: String    -- ^ The actual name of the identifier.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Name: Qualified identifier.
data Name = Name
  { _nmIds  :: [Ident] -- ^ Identifier parts of the name.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

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
  | NullT                -- ^ Type of the null literal, can't be declared.
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Return types (including "void").
type RType = Maybe Type

$(deriveLens [''PrimType, ''Type])

--------------------------------------------------------------------------------
-- Types, Aliases:
--------------------------------------------------------------------------------

-- | Short hand for PrimT BoolT.
boT :: Type
boT = PrimT BoolT

-- | Short hand for PrimT ByteT.
byT :: Type
byT = PrimT ByteT

-- | Short hand for PrimT CharT.
chT :: Type
chT = PrimT CharT

-- | Short hand for PrimT ShortT.
shT :: Type
shT = PrimT ShortT

-- | Short hand for PrimT IntT.
inT :: Type
inT = PrimT IntT

-- | Short hand for PrimT LongT.
loT :: Type
loT = PrimT LongT

-- | Short hand for PrimT FloatT.
flT :: Type
flT = PrimT FloatT

-- | Short hand for PrimT DoubleT.
doT :: Type
doT = PrimT DoubleT

-- | Yields True if the type is primitive numeric.
isTNum :: Type -> Bool
isTNum t = fromMaybe False $ isn't _BoolT <$> (t ^? tPrim)

-- | Yields True if the type is primitive integral.
isTInt :: Type -> Bool
isTInt = (`elem` [byT, chT, shT, inT, loT])

--------------------------------------------------------------------------------
-- Sizability of types:
--------------------------------------------------------------------------------

instance Growable   Type where
  grow = ArrayT

instance Shrinkable Type where
  shrink = \case ArrayT t -> t
                 x        -> x

-- | Dimensionality of a type, an array adds +1 dimensionality.
dimens :: Type -> Integer
dimens = \case
  PrimT _  -> 0
  StringT  -> 0
  ArrayT t -> 1 + dimens t
  NullT    -> 0

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
-- Literals:
--------------------------------------------------------------------------------

-- | Literal values.
data Literal
  = Int {
      _litI  :: Integer -- ^ Literal integer, type is IntT, example: "1".
    }
  | Word {
      _litI  :: Integer -- ^ Literal word, type is LongT, example: "1L".
    }
  | Float {
      _litD  :: Double  -- ^ Literal float, type is FloatT, example: "0.1f".
    }
  | Double {
      _litD  :: Double  -- ^ Literal double, type is DoubleT, example: "0.0".
    }
  | Boolean {
      _litB  :: Bool    -- ^ Literal boolean, type is BoolT, example: "true".
    }
  | Char {
      _litC  :: Char    -- ^ Literal char, type is CharT, example: "'a'".
    }
  | String {
      _litS  :: String  -- ^ Literal String, type is StringT, example: "\"A\"".
    }
  | Null                -- ^ Literal null, type is NullT, example: "null".
  | HoleLiteral {
      _litHole :: Int   -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- lvalues:
--------------------------------------------------------------------------------

-- | LValue: An LValue, as opposed to RValue, is an "expression" that can
-- occur on the left hand side of an assignment. I.e: it models what it means
-- to be a location in memory.
data LValue
  = LVName {
      _lvId    :: Ident   -- ^ Simple variable identifier.
    }
  | LVArray {
      _lvExpr  :: Expr   -- ^ An expression yielding an array.
    , _lvExprs :: [Expr] -- ^ Indexing expressions, must be castable to int.
    }
  | HoleLValue {
      _lvHole  :: Int    -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

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
      _viHole    :: Int       -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | ArrayInit: Initializer of array creation (new T[]...) expressions.
data ArrayInit = ArrayInit
  {
    _aiVIs  :: [VarInit] -- ^ List of initializers for each element in array.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

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
      _eHole     :: Int -- ^ TODO: DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Statements:
--------------------------------------------------------------------------------

-- | VarDeclId: identifier of variable declarations.
data VarDeclId
  = VarDId {
      _vdiIdent :: Ident      -- ^ Identifier of the variable.
    }
  | VarDArr {
      _vdiIdent :: Ident      -- ^ Identifier of the variable.
    , _vdiDimen :: Integer    -- ^ Dimensionality of the variable.
    }
  | HoleVarDeclId {
      _vdiHole   :: Int -- ^ TODO DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | VarDecl: variable declaration (id + initializer).
data VarDecl
  = VarDecl {
      _vdVDI   :: VarDeclId     -- ^ Identifier part of variable.
    , _vdVInit :: Maybe VarInit -- ^ Potential variable initializer.
    }
  | HoleVarDecl {
      _vdHole :: Int -- ^ TODO DOCUMENT THIS.
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | VarMod: A variable can either be declared as final, or not.
data VarMod = VMFinal | VMNormal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

-- | VMType: VarMod + Type.
data VMType
  = VMType {
      _vmMod  :: VarMod -- ^ Modifier of type (final / not).
    , _vmType :: Type   -- ^ Base type of the VMType.
    }
  | HoleVMType {
      _vmHole :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | TypedVVDecl: Typed Variable declarations.
data TypedVVDecl
  = TypedVVDecl {
      _tvdType   :: VMType    -- ^ Type of variable declarations.
    , _tvdVDecls :: [VarDecl] -- ^ List of variable declarations.
    }
  | HoleTypedVVDecl {
      _tvdHole   :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | ForInit: For loop initializer for normal for loops.
data ForInit
  = FIVars {
      _fiVars  :: TypedVVDecl -- ^ For loop variable declarations.
    }
  | FIExprs {
      _fiExprs :: [Expr]      -- ^ For loop initial variable expressions.
    }
  | HoleForInit {
      _fiHole  ::  Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | SwitchLabel (case).
data SwitchLabel
  = SwitchCase {
      _slExpr :: Expr -- ^ Case expression.
    }
  | Default           -- ^ Default case.
  | HoleSwitchLabel {
      _slHole :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | SwitchBlock: One match arm of a switch block.
data SwitchBlock
  = SwitchBlock {
      _sbLab   :: SwitchLabel -- ^ SwitchLabel of the block.
    , _sbBlock :: Block       -- ^ Block of the SwitchBlock.
    }
  | HoleSwitchBlock {
      _sbHole  :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Block of statements.
data Block
  = Block {
      _bStmts :: [Stmt] -- ^ List of statements in the block.
    }
  | HoleBlock {
      _bHole :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

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
      _sHole :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Method:
--------------------------------------------------------------------------------

-- | FormalParam: formal parameter of a method.
data FormalParam = FormalParam
  { _fpType :: VMType    -- ^ Type of parameter.
  , _fpVDI  :: VarDeclId -- ^ Identifier of parameter.
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | MemberDecl: Member declarations of a class.
data MemberDecl
  = MethodDecl {
      _mdRetrT     :: RType         -- ^ Optional return type / void.
    , _mdName      :: Ident         -- ^ Name of method.
    , _mdParams    :: [FormalParam] -- ^ Parameters of method.
    , _mdBlock     :: Block         -- ^ Block of method.
    }
  | HoleMemberDecl {
      _mdHole      :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data MethodBody = MethodBody Block | HoleMethodBody Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Compilation Unit:
--------------------------------------------------------------------------------

-- | Declaration in a class.
data Decl
  = MemberDecl {
      _declMem  :: MemberDecl -- ^ Member declaration of a class.
    }
  | HoleDecl {
      _decHole  :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | ClassBody: class body the class.
data ClassBody
  = ClassBody {
      _cbDecls :: [Decl]  -- ^ Declarations of ClassBody.
    }
  | HoleClassBody {
      _cbHole  :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | ClassDecl: class declaration.
data ClassDecl
  = ClassDecl {
      _cdId   :: Ident       -- ^ Identifier of the class.
    , _cdBody :: ClassBody   -- ^ Body of the class.
    }
  | HoleClassDecl {
      _cdHole :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | TypeDecl: type declarations in the CU.
data TypeDecl
  = ClassTypeDecl {
    _tdClass :: ClassDecl -- ^ Class declaration.
  }
  | HoleTypeDecl {
    _tdHole  :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | CompilationUnit: A whole file.
data CompilationUnit
  = CompilationUnit {
      _cuTDecls :: [TypeDecl] -- ^ Type declarations of the CU.
    }
  | HoleCompilationUnit {
      _cuHole   :: Int
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

--------------------------------------------------------------------------------
-- Derive lenses + prisms:
--------------------------------------------------------------------------------

$(deriveLens [ ''Ident, ''Name
             , ''Literal, ''LValue, ''VarInit, ''ArrayInit, ''Expr
             , ''VarDeclId, ''VMType, ''VarDecl, ''TypedVVDecl, ''Block
             , ''ForInit, ''SwitchLabel, ''SwitchBlock, ''Stmt, ''MemberDecl
             , ''FormalParam, ''CompilationUnit, ''TypeDecl, ''ClassDecl
             , ''ClassBody, ''Decl
             ])