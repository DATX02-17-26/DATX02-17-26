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

{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Core.History where

import Control.Arrow (first)
import Control.Monad ((>=>))

class Revisable t where
  type Label t :: *
  type Orig  t :: *

  forget      :: t -> t
  revise      :: Label t -> t      -> t -> t
  reviseOrig  :: Label t -> Orig t -> t -> t
  revertL     :: t -> Maybe (Label t, Either t (Orig t))

revCount :: Revisable t => t -> Int
revCount = length . fst . revToStartL

revToStartL :: Revisable t => t -> ([Label t], Either t (Orig t))
revToStartL t = maybe ([], Left t)
                      (uncurry $ \l -> either (first (l:) . revToStartL)
                                              (([l],) . pure))
                      (revertL t)

revert :: Revisable t => t -> Maybe (Either t (Orig t))
revert = fmap snd . revertL

revertTL :: Revisable t => t -> Maybe (Label t, t)
revertTL = revertL >=> uncurry (\l -> either (pure . (l,)) (const Nothing))

revertT :: Revisable t => t -> Maybe t
revertT = fmap snd . revertTL

revertO :: Revisable t => t -> Maybe (Orig t)
revertO = fmap snd . revertOL

revertOL :: Revisable t => t -> Maybe ([Label t], Orig t)
revertOL t = let (ls, e) = revToStartL t
             in either (const Nothing) (pure . (ls,)) e



{-

type Label = String

data RevExpr where
    Norm :: Label -> ExprRev t -> RevExpr t
    Orig :: RevExpr t

type Ident = String
data Expr
  = EVar Ident
  | EInt Integer
  | EAdd Expr Expr
  | EMul Expr Expr
  deriving (Eq, Ord, Show, Read)


--------------------------------------------------------------------------------
undoBin :: (() -> Expr t () -> Expr t () -> Expr t ())
        -> RevExpr t -> ExprRev t -> ExprRev t
        -> Expr t ()
undoBin c p l r = case p of
    Norm _ e -> undoNorm e
    Orig     -> c () (undoNorm l) (undoNorm r)

undoNorm :: ExprRev t -> Expr t ()
undoNorm = \case
    EVar v     -> EVar v
    EInt i     -> EInt i
    EAdd p l r -> undoBin EAdd p l r
    EMul p l r -> undoBin EMul p l r

--------------------------------------------------------------------------------
forget :: ExprRev t -> Expr t ()
forget = \case
    EVar v     -> EVar v
    EInt i     -> EInt i
    EAdd _ l r -> EAdd () (forget l) (forget r)
    EMul _ l r -> EMul () (forget l) (forget r)

--------------------------------------------------------------------------------


x, _2, _3 :: Expr Integer a
x  = EVar "x"
_2 = EInt 2
_3 = EInt 3

-- x + x
xpx :: ExprRev Integer
xpx = EAdd Orig x x

-- x + x + x
xpxpx :: ExprRev Integer
xpxpx = EAdd Orig x xpx

-- x + x => 2 * x
norm2x :: ExprRev Integer
norm2x = EMul (Norm "Add2X" xpx) _2 x

-- x + (x + x => 2 * x)
xpnorm2x :: ExprRev Integer
xpnorm2x = EAdd Orig x norm2x

-- x + x + x => 3 * x
norm3x :: ExprRev Integer
norm3x = EMul (Norm "AddXMulCX" xpnorm2x) _3 x
-}