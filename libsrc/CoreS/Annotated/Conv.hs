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

{-# LANGUAGE LambdaCase, TypeFamilies, TypeFamilyDependencies #-}

-- | Conversion to and from Annotated AST.
module CoreS.Annotated.Conv where

import qualified CoreS.AST as S
import qualified CoreS.Annotated.AST as A

--------------------------------------------------------------------------------
-- Conversion class:
--------------------------------------------------------------------------------

-- | Models conversions from and to the annotated AST.
--
-- Laws (assuming annotation is = () ):
-- > toAnnotated . fromAnnotated = id
-- > fromAnnotated . toAnnotated = id
class AnnotatedIso (t :: *) where
  -- | Annotated version of this type.
  type Annotated t = (r :: * -> *) | r -> t

  -- | Converts a term representable in the simple AST to the annotated AST.
  toAnnotated   :: t -> An t

  -- | Converts a term in the annotated AST to one representable in the simple.
  fromAnnotated :: Annotated t a -> t

-- | Annotated AST term with unit, () as annotation.
type An t = Annotated t ()

-- | Lifts a transformation in the Annotated AST into the CoreS.AST.
inAnnotated :: AnnotatedIso t => (An t -> Annotated t a) -> t -> t
inAnnotated f = fromAnnotated . f . toAnnotated

--------------------------------------------------------------------------------
-- Conversion DSL:
--------------------------------------------------------------------------------

(<-$) :: AnnotatedIso t => (An t -> r) -> t -> r
(<-$) f x = f $ toAnnotated x

(<=$) :: (Functor f, AnnotatedIso t) => (f (An t) -> r) -> f t -> r
(<=$) f x = f $ toAnnotated <$> x

(<~$) :: (Functor g, Functor f, AnnotatedIso t)
      => (g (f (An t)) -> r) -> g (f t) -> r
(<~$) f x = f $ fmap toAnnotated <$> x

($->) :: AnnotatedIso t => (t -> r) -> Annotated t a -> r
($->) f x = f $ fromAnnotated x

($=>) :: (Functor f, AnnotatedIso t) => (f t -> r) -> f (Annotated t a) -> r
($=>) f x = f $ fromAnnotated <$> x

($~>) :: (Functor g, Functor f, AnnotatedIso t)
      => (g (f t) -> r) -> g (f (Annotated t a)) -> r
($~>) f x = f $ fmap fromAnnotated <$> x

--------------------------------------------------------------------------------
-- Concrete conversions:
--------------------------------------------------------------------------------

instance AnnotatedIso S.LValue where
  type Annotated S.LValue = A.LValue
  toAnnotated   = \case
    S.LVName n     -> A.LVName  () n
    S.LVArray e es -> A.LVArray () <-$ e <=$ es
    S.HoleLValue h -> A.HoleLValue h

  fromAnnotated = \case
    A.LVName _ n     -> S.LVName n
    A.LVArray _ e es -> S.LVArray $-> e $=> es
    A.HoleLValue h   -> S.HoleLValue h

instance AnnotatedIso S.VarInit where
  type Annotated S.VarInit = A.VarInit
  toAnnotated   = \case
    S.InitExpr e    -> A.InitExpr () <-$ e
    S.InitArr ai    -> A.InitArr  () <-$ ai
    S.HoleVarInit h -> A.HoleVarInit h
  fromAnnotated = \case
    A.InitExpr _ e  -> S.InitExpr $-> e
    A.InitArr _ ai  -> S.InitArr  $-> ai
    A.HoleVarInit h -> S.HoleVarInit h

instance AnnotatedIso S.ArrayInit where
  type Annotated S.ArrayInit = A.ArrayInit
  toAnnotated = \case
    S.ArrayInit vis -> A.ArrayInit () <=$ vis
  fromAnnotated = \case
    A.ArrayInit _ vis -> S.ArrayInit $=> vis

instance AnnotatedIso S.Expr where
  type Annotated S.Expr = A.Expr
  toAnnotated = \case
    S.ELit lit          -> A.ELit () lit
    S.EVar lv           -> A.EVar ()       <-$ lv
    S.ECast t e         -> A.ECast () t    <-$ e
    S.ECond ec ei ee    -> A.ECond ()      <-$ ec      <-$ ei <-$ ee
    S.EAssign lv e      -> A.EAssign ()    <-$ lv      <-$ e
    S.EOAssign lv nop e -> (A.EOAssign ()  <-$ lv) nop <-$ e
    S.ENum op el er     -> A.ENum () op    <-$ el      <-$ er
    S.ECmp op el er     -> A.ECmp () op    <-$ el      <-$ er
    S.ELog op el er     -> A.ELog () op    <-$ el      <-$ er
    S.ENot e            -> A.ENot ()       <-$ e
    S.EStep sop e       -> A.EStep () sop  <-$ e
    S.EBCompl  e        -> A.EBCompl ()    <-$ e
    S.EPlus    e        -> A.EPlus ()      <-$ e
    S.EMinus   e        -> A.EMinus ()     <-$ e
    S.EMApp    n es     -> A.EMApp () n    <=$ es
    S.EInstNew n es     -> A.EInstNew () n <=$ es
    S.EArrNew  t es i   -> A.EArrNew () t  <=$ es $ i
    S.EArrNewI t i ai   -> A.EArrNewI () t i <-$ ai
    S.ESysOut  e        -> A.ESysOut ()    <-$ e
    S.HoleExpr i        -> A.HoleExpr i
  fromAnnotated = \case
    A.ELit _ lit          -> S.ELit lit
    A.EVar _ lv           -> S.EVar      $-> lv
    A.ECast _ t e         -> S.ECast t   $-> e
    A.ECond _ ec ei ee    -> S.ECond     $-> ec      $-> ei $-> ee
    A.EAssign _ lv e      -> S.EAssign   $-> lv      $-> e
    A.EOAssign _ lv nop e -> (S.EOAssign $-> lv) nop $-> e
    A.ENum _ op el er     -> S.ENum op   $-> el      $-> er
    A.ECmp _ op el er     -> S.ECmp op   $-> el      $-> er
    A.ELog _ op el er     -> S.ELog op   $-> el      $-> er
    A.ENot _ e            -> S.ENot      $-> e
    A.EStep _ sop e       -> S.EStep sop $-> e
    A.EBCompl _  e        -> S.EBCompl   $-> e
    A.EPlus _    e        -> S.EPlus     $-> e
    A.EMinus _   e        -> S.EMinus    $-> e
    A.EMApp _ n es        -> S.EMApp n   $=> es
    A.EInstNew _ n es     -> S.EInstNew n $=> es
    A.EArrNew _  t es i   -> S.EArrNew t $=> es $ i
    A.EArrNewI _ t i ai   -> S.EArrNewI t i $-> ai
    A.ESysOut _  e        -> S.ESysOut   $-> e
    A.HoleExpr i          -> S.HoleExpr i

instance AnnotatedIso S.VarDecl where
  type Annotated S.VarDecl = A.VarDecl
  toAnnotated   = \case
    S.VarDecl vdi mvi -> A.VarDecl () vdi <=$ mvi
    S.HoleVarDecl h   -> A.HoleVarDecl h
  fromAnnotated = \case
    A.VarDecl _ vdi mvi -> S.VarDecl vdi $=> mvi
    A.HoleVarDecl h     -> S.HoleVarDecl h

instance AnnotatedIso S.TypedVVDecl where
  type Annotated S.TypedVVDecl = A.TypedVVDecl
  toAnnotated   = \case
    S.TypedVVDecl t vds -> A.TypedVVDecl () t <=$ vds
    S.HoleTypedVVDecl h -> A.HoleTypedVVDecl h
  fromAnnotated = \case
    A.TypedVVDecl _ t vds -> S.TypedVVDecl t $=> vds
    A.HoleTypedVVDecl h   -> S.HoleTypedVVDecl h

instance AnnotatedIso S.ForInit where
  type Annotated S.ForInit = A.ForInit
  toAnnotated = \case
    S.FIVars v      -> A.FIVars () <-$ v
    S.FIExprs es    -> A.FIExprs () <=$ es
    S.HoleForInit i -> A.HoleForInit i
  fromAnnotated = \case
    A.FIVars _ v    -> S.FIVars $-> v
    A.FIExprs _ es  -> S.FIExprs $=> es
    A.HoleForInit i -> S.HoleForInit i

instance AnnotatedIso S.SwitchLabel where
  type Annotated S.SwitchLabel = A.SwitchLabel
  toAnnotated   = \case
    S.SwitchCase e      -> A.SwitchCase () <-$ e
    S.Default           -> A.Default
    S.HoleSwitchLabel h -> A.HoleSwitchLabel h
  fromAnnotated = \case
    A.SwitchCase _ e    -> S.SwitchCase $-> e
    A.Default           -> S.Default
    A.HoleSwitchLabel h -> S.HoleSwitchLabel h

instance AnnotatedIso S.SwitchBlock where
  type Annotated S.SwitchBlock = A.SwitchBlock
  toAnnotated   = \case
    S.SwitchBlock l bs   -> A.SwitchBlock () <-$ l <-$ bs
    S.HoleSwitchBlock i  -> A.HoleSwitchBlock i
  fromAnnotated = \case
    A.SwitchBlock _ l bs -> S.SwitchBlock $-> l $-> bs
    A.HoleSwitchBlock i  -> S.HoleSwitchBlock i

instance AnnotatedIso S.Block where
  type Annotated S.Block = A.Block
  toAnnotated   = \case
    S.Block ss    -> A.Block () <=$ ss
    S.HoleBlock i -> A.HoleBlock i
  fromAnnotated = \case
    A.Block _ ss  -> S.Block $=> ss
    A.HoleBlock i -> S.HoleBlock i

instance AnnotatedIso S.Stmt where
  type Annotated S.Stmt = A.Stmt
  toAnnotated = \case
    S.SEmpty              -> A.SEmpty ()
    S.SBlock bs           -> A.SBlock ()    <-$ bs
    S.SExpr e             -> A.SExpr ()     <-$ e
    S.SVars t             -> A.SVars ()     <-$ t
    S.SReturn e           -> A.SReturn ()   <-$ e
    S.SVReturn            -> A.SVReturn ()
    S.SIf     e si        -> A.SIf ()       <-$ e <-$ si
    S.SIfElse e si se     -> A.SIfElse ()   <-$ e <-$ si <-$ se
    S.SWhile  e si        -> A.SWhile ()    <-$ e <-$ si
    S.SDo     e si        -> A.SDo ()       <-$ e <-$ si
    S.SForE t i e si      -> A.SForE () t i <-$ e <-$ si
    S.SForB mfi me mes si -> A.SForB ()     <=$ mfi <=$ me <~$ mes <-$ si
    S.SContinue           -> A.SContinue ()
    S.SBreak              -> A.SBreak ()
    S.SSwitch e lst       -> A.SSwitch ()   <-$ e <=$ lst
    S.HoleStmt i          -> A.HoleStmt i
  fromAnnotated = \case
    A.SEmpty _              -> S.SEmpty
    A.SBlock _ bs           -> S.SBlock    $-> bs
    A.SExpr _ e             -> S.SExpr     $-> e
    A.SVars _ t             -> S.SVars     $-> t
    A.SReturn _ e           -> S.SReturn   $-> e
    A.SVReturn _            -> S.SVReturn
    A.SIf _     e si        -> S.SIf       $-> e $-> si
    A.SIfElse _ e si se     -> S.SIfElse   $-> e $-> si $-> se
    A.SWhile _  e si        -> S.SWhile    $-> e $-> si
    A.SDo _     e si        -> S.SDo       $-> e $-> si
    A.SForE _ t i e si      -> S.SForE t i $-> e $-> si
    A.SForB _ mfi me mes si -> S.SForB     $=> mfi $=> me $~> mes $-> si
    A.SContinue _           -> S.SContinue
    A.SBreak _              -> S.SBreak
    A.SSwitch _ e lst       -> S.SSwitch   $-> e $=> lst
    A.HoleStmt i            -> S.HoleStmt i

instance AnnotatedIso S.MemberDecl where
  type Annotated S.MemberDecl = A.MemberDecl
  toAnnotated = \case
    S.MethodDecl m i fps bs -> A.MethodDecl () m i fps <-$ bs
    S.HoleMemberDecl i      -> A.HoleMemberDecl i
  fromAnnotated = \case
    A.MethodDecl _ m i fps bs -> S.MethodDecl m i fps $-> bs
    A.HoleMemberDecl i        -> S.HoleMemberDecl i

instance AnnotatedIso S.Decl where
  type Annotated S.Decl = A.Decl
  toAnnotated = \case
    S.MemberDecl m -> A.MemberDecl () <-$ m
    S.HoleDecl i   -> A.HoleDecl i
  fromAnnotated = \case
    A.MemberDecl _ m -> S.MemberDecl $-> m
    A.HoleDecl i     -> S.HoleDecl i

instance AnnotatedIso S.ClassBody where
  type Annotated S.ClassBody = A.ClassBody
  toAnnotated = \case
    S.ClassBody decls -> A.ClassBody () <=$ decls
    S.HoleClassBody i -> A.HoleClassBody i
  fromAnnotated = \case
    A.ClassBody _ decls -> S.ClassBody $=> decls
    A.HoleClassBody i   -> S.HoleClassBody i

instance AnnotatedIso S.ClassDecl where
  type Annotated S.ClassDecl = A.ClassDecl
  toAnnotated = \case
    S.ClassDecl i body -> A.ClassDecl () i <-$ body
    S.HoleClassDecl i  -> A.HoleClassDecl i
  fromAnnotated = \case
    A.ClassDecl _ i body -> S.ClassDecl i $-> body
    A.HoleClassDecl i    -> S.HoleClassDecl i

instance AnnotatedIso S.TypeDecl where
  type Annotated S.TypeDecl = A.TypeDecl
  toAnnotated = \case
    S.ClassTypeDecl cls -> A.ClassTypeDecl () <-$ cls
    S.HoleTypeDecl i    -> A.HoleTypeDecl i
  fromAnnotated = \case
    A.ClassTypeDecl _ cls -> S.ClassTypeDecl $-> cls
    A.HoleTypeDecl i      -> S.HoleTypeDecl i

instance AnnotatedIso S.ImportDecl where
  type Annotated S.ImportDecl = A.ImportDecl
  toAnnotated = \case
    S.ImportDecl n s w -> A.ImportDecl () n s w
    S.HoleImportDecl i -> A.HoleImportDecl i
  fromAnnotated = \case
    A.ImportDecl _ n s w -> S.ImportDecl n s w
    A.HoleImportDecl i   -> S.HoleImportDecl i

instance AnnotatedIso S.CompilationUnit where
  type Annotated S.CompilationUnit = A.CompilationUnit
  toAnnotated = \case
    S.CompilationUnit is tds -> A.CompilationUnit () <=$ is <=$ tds
    S.HoleCompilationUnit i  -> A.HoleCompilationUnit i
  fromAnnotated = \case
    A.CompilationUnit _ is tds -> S.CompilationUnit $=> is $=> tds
    A.HoleCompilationUnit i    -> S.HoleCompilationUnit i