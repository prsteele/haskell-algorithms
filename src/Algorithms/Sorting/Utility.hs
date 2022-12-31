{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Algorithms.Sorting.Utility where

import Data.Ord (comparing)
import qualified Data.Vector.Generic as G

-- | Transform a sort-by function into a sort-on function.
mkSortOn ::
  (G.Vector v a, G.Vector v (b, a), Ord b) =>
  -- | The sort-by function
  (((b, a) -> (b, a) -> Ordering) -> v (b, a) -> v (b, a)) ->
  -- | The sort-on function
  ((a -> b) -> v a -> v a)
{-# INLINEABLE mkSortOn #-}
mkSortOn sortBy = \f v ->
  let tag x = let y = f x in y `seq` (y, x)
   in G.map snd (sortBy (comparing fst) (G.map tag v))
