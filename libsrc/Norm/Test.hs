{-# LANGUAGE LambdaCase #-}

module Norm.Test where

import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Query.DFS


import Test.QuickCheck (quickCheck)
import Data.List (sort, foldl1)
import Data.Function (fix)

u = undefined

data W
  = Requires     -- ^ Denotes that a rule depends on another.
  | Invalidates  -- ^ Denotes that a rule invalidates another rule.
  deriving (Eq, Show)

{-

fboth :: Functor f => (a -> b) -> (f a, f a) -> (f b, f b)
fboth f = fmap f *** fmap f

outs :: Graph gr => gr a b -> Node -> (Adj b, a)
outs gr n = let (_, _, l, o) = fromJust $ fst $ match n gr in (o, l)

starts :: Graph gr => gr a b -> [(Adj b, a)]
starts gr = filter (not . null . fst) $ outs gr <$> nodes gr

partW :: Adj W -> (Adj W, Adj W)
partW = partition ((Requires ==) . fst)

linearise :: Graph gr => gr a W -> [a]
linearise gr = concat $ linearise' gr <$> starts gr

linearise' :: Graph gr => gr a W -> (Adj W, a) -> [a]
linearise' gr (o, a) = concat req ++ [a] ++ concat inv
  where (req, inv) = fboth (linearise' gr . outs gr . snd) $ partW o


-- | Remove consecutive elements which are equal to a previous element.
-- Runtime complexity: O(n), space: O(1)
removeConsequtiveEq :: Eq a => [a] -> [a]
removeConsequtiveEq = \case
  []    -> []
  [x]   -> [x]
  (h:t) -> h : ug h t
  where
    ug e = \case
      []     -> []
      (x:xs) | e == x    ->     ug x xs
      (x:xs) | otherwise -> x : ug x xs


toMDG :: [MkEdge] -> Gr Int Bool
toMDG edges = toGraph edges $ (\(MkEdge w f t) -> (f, t, w)) <$> edges

-}

data MkEdge = MkEdge Bool Int Int
req = MkEdge True
inv = MkEdge False

toGraph :: [MkEdge] -> [(Int, Int, Bool)] -> Gr Int Bool
toGraph edges es = run_ empty nm
  where ns = nub $ edges >>= \(MkEdge _ f t) -> [f, t]
        nm = insMapNodesM ns >> insMapEdgesM es

-- | Make graph into a directed cyclic graph (DCG).
-- "Requires"    denotes a forward  edge.
-- "Invalidates" denotes a backward edge.
toDCG :: [MkEdge] -> Gr Int Bool
toDCG edges = toGraph edges $
  (\(MkEdge w f t) -> if w then (t, f, w) else (f, t, w)) <$> edges

-- | Make a palindrome of the given list by computing: [1 .. n] ++ [n - 1 .. 1].
-- Runtime complexity: O(n).
palindrome :: [a] -> [a]
palindrome = \case
  [] -> []
  xs -> xs ++ tail (reverse xs)

linearize :: Gr Int a -> [Int]
linearize dcg = concat $ topsort' scc
  where scc  = nmap (palindrome . fmap (fromJust . lab dcg)) $ condensation dcg



g1 = [ 1 `req` 0
     , 4 `inv` 0
     , 2 `req` 0
     , 5 `inv` 0
     , 3 `req` 0
     , 6 `inv` 0
     ]

g2 = [ 2 `req` 1
     , 2 `inv` 1
     , 3 `req` 1
     , 3 `inv` 1
     , 4 `req` 1
     , 5 `inv` 1
     ]

g2e = [ (1, 2)
      , (2, 1)
      , (1, 3)
      , (3, 1)
      , (1, 4)
      , (5, 1)
      ]

-- [1, 4, 2, 1, 3, 5]
-- [5, 1, 4, 2, 1, 3]


remove :: Eq a => a -> [a] -> [a] 
remove a = \case
  []     -> []
  (x:xs) -> if a == x then xs else x : remove a xs

smallest :: Ord a => [a] -> a
smallest = foldl1 $ \x y -> if x < y then x else y

sort' :: Ord a => [a] -> [a]
sort' = fix $ \f -> \case
  [] -> [ ]
  xs -> let s = smallest xs in s : f (remove s xs)

prop_sort :: Ord a => [a] -> Bool
prop_sort xs = sort xs == sort' xs


isSingleton :: [x] -> Bool
isSingleton = \case
  [x] -> True;
  _   -> False;

singletonLists :: [[x]] -> [[x]]
singletonLists = filter isSingleton