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

-- | Revisable class of types.
module Core.History (
  -- * Classes and types
    Revisable
  , Label
  , Orig
  -- * Class functions
  , forgetTop
  , forget
  , revise
  , reviseOrig
  , revertL
  -- * Free utility functions
  , revCount
  , revToStartL
  , eitherId
  , revSameStart
  , revert
  , revertSame
  , revertTL
  , revertT
  , revertO
  , revertOL
  , rev2
  ) where

import Control.Arrow (first, second, (***))
import Control.Monad ((>=>))

--------------------------------------------------------------------------------
-- Class:
--------------------------------------------------------------------------------

-- | Class of types (:: *) that are revisable.
-- Revisions have a Label, and a type for the original (0th) version
-- This type (Orig) may or may not be the same type as t.
class Revisable (t :: *) where
  type Label t :: *
  type Orig  t :: *

  -- | Forget history in a term on the top level only.
  -- Applying this to a term twice must have no additional effect.
  -- Formally: forgetTop . forgetTop == forgetTop
  forgetTop  :: t -> t

  -- | Forgets history in all places where there is a history.
  -- Applying this to a term twice must have no additional effect.
  -- Formally: forget . forget == forget
  forget     :: t -> t

  -- | Revises the history of the last argument to be that of the second arg,
  -- and labled with the first arg.
  -- The implementation may overwrite the history of the third argument.
  revise     :: Label t -> t      -> t -> t

  -- | Revises the history of the last argument to be originally that of the
  -- second arg and lables the change with the first arg.
  -- The implementation must overwrite the history of the third argument.
  reviseOrig :: Label t -> Orig t -> t -> t

  -- | Reverts one change in the history of the given term.
  -- If Nothing is yielded, then there exists no history and the given term
  -- was the beginning. If Just is yielded, then the label is in fst while
  -- snd is either the current or original type. It may be that t == Orig t,
  -- in which case eitherId can be used to eliminate the Either.
  revertL    :: t -> Maybe (Label t, Either t (Orig t))

--------------------------------------------------------------------------------
-- Extra functions:
--------------------------------------------------------------------------------

-- | Yields the number of revisions made to the given term.
-- The count is shallow, i.e: it does not traverse sub-terms and accumulate.
revCount :: Revisable t => t -> Int
revCount = length . fst . revToStartL

-- | Reverts all revisions that can be reverted in the term t and yields the
-- most original term + the all change labels.
revToStartL :: Revisable t => t -> ([Label t], Either t (Orig t))
revToStartL t = maybe ([], Left t)
                      (uncurry $ \l ->
                        either (first (l:) . revToStartL) (([l],) . pure))
                      (revertL t)

-- | Eliminates Either a a to a.
eitherId :: Either a a -> a
eitherId = either id id

-- | See revToStartL
revSameStart :: (Revisable t, t ~ Orig t) => t -> ([Label t], t)
revSameStart = second eitherId . revToStartL

-- | Reverts once, but ignores the label.
revert :: Revisable t => t -> Maybe (Either t (Orig t))
revert = fmap snd . revertL

-- | see revert.
revertSame :: (Revisable t, t ~ Orig t) => t -> Maybe t
revertSame = fmap eitherId . revert

-- | reverts one non-original revision.
revertTL :: Revisable t => t -> Maybe (Label t, t)
revertTL = revertL >=> uncurry (\l -> either (pure . (l,)) (const Nothing))

-- | see revertTL.
revertT :: Revisable t => t -> Maybe t
revertT = fmap snd . revertTL

-- | reverts one original revision.
revertO :: Revisable t => t -> Maybe (Orig t)
revertO = fmap snd . revertOL

-- | reverts until the original revision if possible.
revertOL :: Revisable t => t -> Maybe ([Label t], Orig t)
revertOL t = let (ls, e) = revToStartL t
             in either (const Nothing) (pure . (ls,)) e

-- | composes reversion for two linked Revisable:s.
rev2 :: (Revisable a, Revisable b, Orig a ~ b, Label a ~ Label b)
     => a -> ([Label a], Either a (Either b (Orig b)))
rev2 a = let (ls1, e1) = revToStartL a
         in  either ((ls1,) . Left) (((ls1 ++) *** Right) . revToStartL) e1

--------------------------------------------------------------------------------
-- Temporary example:
--------------------------------------------------------------------------------

data RExpr
  = Revi String Expr
  | NC
  deriving (Eq, Ord, Show, Read)

data Expr
  = EVar RExpr String
  | EInt RExpr Integer
  | EAdd RExpr Expr Expr
  | EMul RExpr Expr Expr
  deriving (Eq, Ord, Show, Read)

modRev :: (RExpr -> RExpr) -> Expr -> Expr
modRev f = \case
  EVar rv s   -> EVar (f rv) s
  EInt rv i   -> EInt (f rv) i
  EAdd rv l r -> EAdd (f rv) l r
  EMul rv l r -> EMul (f rv) l r

rev :: RExpr -> Maybe (String, Either Expr Expr)
rev = \case
  NC -> Nothing
  Revi l h -> Just (l, Left h)

instance Revisable Expr where
  type Label Expr = String
  type Orig  Expr = Expr
  reviseOrig = revise
  revise l h = modRev $ const $ Revi l h
  forgetTop  = modRev $ const NC
  forget     = \case
    EVar _ s   -> EVar NC s
    EInt _ i   -> EInt NC i
    EAdd _ l r -> EAdd NC (forget l) (forget r)
    EMul _ l r -> EMul NC (forget l) (forget r)
  revertL    = \case
    EVar rv _   -> rev rv
    EInt rv _   -> rev rv
    EAdd rv _ _ -> rev rv
    EMul rv _ _ -> rev rv