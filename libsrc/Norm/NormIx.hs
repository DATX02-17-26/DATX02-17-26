{-# LANGUAGE DeriveDataTypeable, DeriveGeneric
  , TemplateHaskell
  , LambdaCase
  , FlexibleContexts
  , RankNTypes #-}

module Norm.NormIx where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

import Data.Monoid ((<>))
import Data.List (uncons)
import Data.Tree (Tree (..), subForest, rootLabel)
import Data.Functor.Compose (Compose (Compose))
import Control.Arrow (first)
import Control.Monad.Trans.State (state, runState)
import Data.Data.Lens (uniplate, biplate)
import Control.Lens (Traversal', IndexedTraversal',
  indexed, imapMOf, auf, iso, _Unwrapping)

import Control.Lens hiding (uncons)

import Util.Debug
import Util.TH

import Class.HasError (HasError, toEither)
import Norm.NormM hiding (state)

u = undefined

--------------------------------------------------------------------------------
-- IndexedTraversal construction from list of indices:
--------------------------------------------------------------------------------

-- | Creates an IndexedTraversal using the given indices.
-- This is only for the immediate level.
withIndices :: Traversal' s a -> [i] -> IndexedTraversal' i s a
withIndices l is f x = fst $ auf (iso state runState . _Unwrapping Compose) l
  (\a -> maybe (pure a, []) (first (flip (indexed f) a)) . uncons)
  x is

--------------------------------------------------------------------------------
-- IndexedTraversal versions of transformMOf, etc.:
--------------------------------------------------------------------------------

-- | Generalized indexed version of transformMOf.
itransformMOf :: Monad m
              => (i -> IndexedTraversal' i a a)
              -> (i -> a -> m a) -> i -> a -> m a
itransformMOf trav f = let go i t = imapMOf (trav i) go t >>= f i in go

-- | Generalized indexed version of itransformMOnOf.
itransformMOnOf :: Monad m
  => (i -> IndexedTraversal' i s a)
  -> (i -> IndexedTraversal' i a a)
  -> (i -> a -> m a) -> i -> s -> m s
itransformMOnOf b l f i = imapMOf (b i) $ itransformMOf l f

--------------------------------------------------------------------------------
-- Bottom up traversals with a rose tree as indices.
--------------------------------------------------------------------------------

-- | Creates an indexed traversal with indices from the subForest of the given
-- rose tree. This structure must correspond to the given Traversal' s a,
-- or the modified result will be smaller.
sfIx :: Traversal' s a -> Tree i -> IndexedTraversal' (Tree i) s a
sfIx t = withIndices t . subForest

-- | Monadic bottom up indexed traversal of a structure with
-- indices from a rose tree that corresponds 1:1 with the thing to traverse.
-- Indexed version of transformMOf.
rtTransformMOf :: Monad m
          => Traversal' a a
          -> (Tree i -> a -> m a) -> Tree i -> a -> m a
rtTransformMOf t = itransformMOf $ sfIx t

-- | Monadic bottom up indexed traversal of a structure with
-- indices from a rose tree that corresponds 1:1 with the thing to traverse.
-- Indexed version of transformMOnOf.
rtTransformMOnOf :: Monad m
            => Traversal' s a -> Traversal' a a
            -> (Tree i -> a -> m a) -> Tree i -> s -> m s
rtTransformMOnOf b l = itransformMOnOf (sfIx b) (sfIx l)

-- | Indexed version of normEveryT using a rose tree for decoration (index).
transformMU :: (Monad m, Data a)
            => (Tree i -> a -> m a) -> Tree i -> a -> m a
transformMU = rtTransformMOf uniplate

-- | Indexed version of normEvery using a rose tree for decoration (index).
transformMB :: (Monad m, Data s, Data a)
            => (Tree i -> a -> m a) -> Tree i -> s -> m s
transformMB = rtTransformMOnOf biplate uniplate

-- | Indexed version of normEveryT using a rose tree for decoration (index).
-- The rose tree is produced by a function given the top level term.
ixNormEveryT :: (HasError e n, Monad m, Data a)
            => (a -> n (Tree i))
            -> (Tree i -> a -> m a)
            -> a -> m a
ixNormEveryT mkTree f a =
  either (const $ pure a)
         (flip (transformMU f) a)
         (toEither $ mkTree a)

-- | Indexed version of normEvery using a rose tree for decoration (index).
-- The rose tree is produced by a function given the top level term.
ixNormEvery :: (HasError e n, Monad m, Data s, Data a)
            => (s -> n (Tree i))
            -> (Tree i -> a -> m a)
            -> s -> m s
ixNormEvery mkTree f s =
  either (const $ pure s)
         (flip (transformMB f) s)
         (toEither $ mkTree s)

--------------------------------------------------------------------------------
-- Testing:
--------------------------------------------------------------------------------

data Purity = Pure | Impure
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Typeable, Data, Generic)

instance Monoid Purity where
  mempty      = Pure
  mappend x y = if y > x then y else x

type Dec d = Tree (Maybe d)

mkDec :: d -> Dec d
mkDec = pure . pure

data E
  = P
  | IP
  | Add {
      _lft :: E
    , _rgt :: E
    }
  | Abc {
      _one :: E
    , _mid :: Int
    , _two :: E
    }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

(|+|) = Add

deriveLens [''E]

lftIx :: IndexedTraversal' Int E E
lftIx = lft . flip indexed (0 :: Int)





decPurity :: E -> Dec Purity
decPurity = \case
  P       -> mkDec Pure
  IP      -> mkDec Impure
  Add l r -> do
    let tl = decPurity l
    let tr = decPurity r
    let pl = rootLabel tl
    let pr = rootLabel tr
    let pc = pl <> pr
    Node pc [tl, tr]



e2 = Abc P 1 P

e :: E
e = P |+| IP |+| P

n1 = Node 1 [Node 2 []]
n2 = Node 1 []

mkTree :: E -> Maybe (Tree Bool)
mkTree = pure . fmap (== Just Pure) . decPurity

--getter :: Lens' 
--fetch :: Lens' -> Dec -> Dec

test :: E -> IO E
test = ixNormEveryT mkTree $ \md x -> do

  print [show $ rootLabel md, show x]
  pure $ if x == IP then P else x