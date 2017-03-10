module Data.RoseTree where

import Data.Monoid ((<>))

--Create a Rose Tree Data Type
data RoseTree a = Node a [RoseTree a]

--Creating Functor, Applicative and Monad for the Rose Tree
instance Functor RoseTree where
  fmap f (Node a rs) = Node (f a) (fmap f <$> rs)

instance Applicative RoseTree where
    pure x =
      Node x []
    (<*>) (Node f fs) x@(Node y ys) =
        Node (f y) $ fmap (<*> x) fs <> fmap (f <$>) ys

instance Monad RoseTree where
  return = pure

  (>>=) (Node x xs) k =
    let Node y ys = k x in
      Node y $ fmap (>>= k) xs <> ys

