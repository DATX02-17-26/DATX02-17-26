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

{-# LANGUAGE TupleSections
  , KindSignatures, TypeFamilies, TypeFamilyDependencies
  , RankNTypes
  , ScopedTypeVariables
  #-}

-- | Product types that know how to index themselves and un-index themselves.
module Class.Product.IxProd (
  -- * Classes
    IxProd
  -- * Types
  , PIx
  -- * Primitive operations
  , pIxEn
  , pUnIx
  , pIxXs
  -- * Derived operations
  , pReIx
  , ixHead
  ) where

import Data.Maybe (fromJust)
import Data.List (uncons)
import Data.Proxy (Proxy (Proxy))
import Data.Function.Pointless ((.:))
import Control.Arrow (first)
import Control.Lens (Prism', (^?), (&))

--------------------------------------------------------------------------------
-- Class:
--------------------------------------------------------------------------------

-- | A product type that knows how to index itself and un-index itself.
class IxProd p where
  -- | The indexed version of the product type p with index i.
  type PIx (p :: *) (i :: *) = (r :: *) | r -> p

  -- | Un-indexes the given product type.
  -- Requires a type witness for the index.
  pUnIx :: Proxy i -> PIx p i -> p

  -- | Produces an indexed version of the given product type
  -- with the indices taken from a given list.
  -- If not enough indices exists, the result should be Nothing.
  pIxXs :: [i] -> p -> Maybe (PIx p i)

  -- | Reindexes an already indexed given product type
  -- with the indices taken from a given list.
  -- If not enough indices exists, the result should be Nothing.
  -- Requires a type witness for the index.
  pReIx :: Proxy i -> [j] -> PIx p i -> Maybe (PIx p j)
  pReIx p is pr = pIxXs is $ pUnIx p pr

  -- | Produces an indexed version of the given product type
  -- for an index type that is enumerable.
  -- Requires a type witness for the index.
  pIxEn :: Enum i => Proxy i -> p -> PIx p i
  pIxEn (Proxy :: Proxy i) pr = fromJust $ pIxXs (enumFrom $ toEnum 0 :: [i]) pr

--------------------------------------------------------------------------------
-- Auxilary functions:
--------------------------------------------------------------------------------

-- | Performs an safe head of a given Prism.
-- It adds the indices from the given list.
ixHead :: IxProd p => [i] -> s -> Prism' s p -> Maybe (PIx p i)
ixHead is = (>>= pIxXs is) .: (^?)

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

(<-:) :: x -> [i] -> Maybe ((x, i), [i])
(<-:) x is = first (x,) <$> uncons is

instance IxProd (x0, x1) where
  type PIx
    (  x0
    ,  x1
    ) i =
    ( (x0, i)
    , (x1, i)
    )
  pUnIx p
    ( x0
    , x1
    ) =
    ( x0 & fst
    , x1 & fst
    )
  pIxXs is0
    ( x0
    , x1
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    pure (y0, y1)

instance IxProd (x0, x1, x2) where
  type PIx
    (  x0
    ,  x1
    ,  x2
    ) i =
    ( (x0, i)
    , (x1, i)
    , (x2, i)
    )
  pUnIx p
    ( x0
    , x1
    , x2
    ) =
    ( x0 & fst
    , x1 & fst
    , x2 & fst
    )
  pIxXs is0
    ( x0
    , x1
    , x2
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    (y2, is3) <- x2 <-: is2
    pure (y0, y1, y2)

instance IxProd (x0, x1, x2, x3) where
  type PIx
    (  x0
    ,  x1
    ,  x2
    ,  x3
    ) i =
    ( (x0, i)
    , (x1, i)
    , (x2, i)
    , (x3, i)
    )
  pUnIx p
    ( x0
    , x1
    , x2
    , x3
    ) =
    ( x0 & fst
    , x1 & fst
    , x2 & fst
    , x3 & fst
    )
  pIxXs is0
    ( x0
    , x1
    , x2
    , x3
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    (y2, is3) <- x2 <-: is2
    (y3, is4) <- x3 <-: is3
    pure (y0, y1, y2, y3)

instance IxProd (x0, x1, x2, x3, x4) where
  type PIx
    (  x0
    ,  x1
    ,  x2
    ,  x3
    ,  x4
    ) i =
    ( (x0, i)
    , (x1, i)
    , (x2, i)
    , (x3, i)
    , (x4, i)
    )
  pUnIx p
    ( x0
    , x1
    , x2
    , x3
    , x4
    ) =
    ( x0 & fst
    , x1 & fst
    , x2 & fst
    , x3 & fst
    , x4 & fst
    )
  pIxXs is0
    ( x0
    , x1
    , x2
    , x3
    , x4
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    (y2, is3) <- x2 <-: is2
    (y3, is4) <- x3 <-: is3
    (y4, is5) <- x4 <-: is4
    pure (y0, y1, y2, y3, y4)

instance IxProd (x0, x1, x2, x3, x4, x5) where
  type PIx
    (  x0
    ,  x1
    ,  x2
    ,  x3
    ,  x4
    ,  x5
    ) i =
    ( (x0, i)
    , (x1, i)
    , (x2, i)
    , (x3, i)
    , (x4, i)
    , (x5, i)
    )
  pUnIx p
    ( x0
    , x1
    , x2
    , x3
    , x4
    , x5
    ) =
    ( x0 & fst
    , x1 & fst
    , x2 & fst
    , x3 & fst
    , x4 & fst
    , x5 & fst
    )
  pIxXs is0
    ( x0
    , x1
    , x2
    , x3
    , x4
    , x5
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    (y2, is3) <- x2 <-: is2
    (y3, is4) <- x3 <-: is3
    (y4, is5) <- x4 <-: is4
    (y5, is6) <- x5 <-: is5
    pure (y0, y1, y2, y3, y4, y5)

instance IxProd (x0, x1, x2, x3, x4, x5, x6) where
  type PIx
    (  x0
    ,  x1
    ,  x2
    ,  x3
    ,  x4
    ,  x5
    ,  x6
    ) i =
    ( (x0, i)
    , (x1, i)
    , (x2, i)
    , (x3, i)
    , (x4, i)
    , (x5, i)
    , (x6, i)
    )
  pUnIx p
    ( x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    ) =
    ( x0 & fst
    , x1 & fst
    , x2 & fst
    , x3 & fst
    , x4 & fst
    , x5 & fst
    , x6 & fst
    )
  pIxXs is0
    ( x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    (y2, is3) <- x2 <-: is2
    (y3, is4) <- x3 <-: is3
    (y4, is5) <- x4 <-: is4
    (y5, is6) <- x5 <-: is5
    (y6, is7) <- x6 <-: is6
    pure (y0, y1, y2, y3, y4, y5, y6)

instance IxProd (x0, x1, x2, x3, x4, x5, x6, x7) where
  type PIx
    (  x0
    ,  x1
    ,  x2
    ,  x3
    ,  x4
    ,  x5
    ,  x6
    ,  x7
    ) i =
    ( (x0, i)
    , (x1, i)
    , (x2, i)
    , (x3, i)
    , (x4, i)
    , (x5, i)
    , (x6, i)
    , (x7, i)
    )
  pUnIx p
    ( x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    , x7
    ) =
    ( x0 & fst
    , x1 & fst
    , x2 & fst
    , x3 & fst
    , x4 & fst
    , x5 & fst
    , x6 & fst
    , x7 & fst
    )
  pIxXs is0
    ( x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    , x7
    ) = do
    (y0, is1) <- x0 <-: is0
    (y1, is2) <- x1 <-: is1
    (y2, is3) <- x2 <-: is2
    (y3, is4) <- x3 <-: is3
    (y4, is5) <- x4 <-: is4
    (y5, is6) <- x5 <-: is5
    (y6, is7) <- x6 <-: is6
    (y7, is8) <- x7 <-: is7
    pure (y0, y1, y2, y3, y4, y5, y6, y7)