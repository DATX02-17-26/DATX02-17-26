module Data.RoseTree where

import Data.Monoid ((<>))

--Create a Rose Tree Data Type
data RoseTree a = RoseTree { root :: a, branches :: [RoseTree a] } deriving (Eq, Ord, Show)

--Creating Functor, Applicative and Monad for the Rose Tree
instance Functor RoseTree where
  fmap f (RoseTree a rs) = RoseTree (f a) (fmap f <$> rs)

instance Applicative RoseTree where
  pure x = RoseTree x []

  (RoseTree f fs) <*> x@(RoseTree y ys) =
      RoseTree (f y) $ fmap (<*> x) fs <> fmap (f <$>) ys

instance Monad RoseTree where
  return = pure

  (RoseTree x xs) >>= k =
    let RoseTree y ys = k x in
      RoseTree y $ fmap (>>= k) xs <> ys

-- Filter a rose tree based on a predicate, always leave
-- the root node in place.
filterTree :: RoseTree a -> (a -> Bool) -> RoseTree a
filterTree (RoseTree a trees) p =
  RoseTree a [filterTree t p | t@(RoseTree a' _) <- trees, p a']
