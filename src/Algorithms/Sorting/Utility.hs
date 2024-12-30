{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Algorithms.Sorting.Utility where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Ord (comparing)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Transform a sort-by function into a sort-on function.
mkSortOn ::
  (G.Vector v a, G.Vector v (b, a), Ord b) =>
  -- | The sort-by sorting algorithm
  (((b, a) -> (b, a) -> Ordering) -> v (b, a) -> v (b, a)) ->
  -- | The sort-on function
  ((a -> b) -> v a -> v a)
{-# INLINEABLE mkSortOn #-}
mkSortOn sortBy = \f v ->
  let tag x = let y = f x in y `seq` (y, x)
   in G.map snd (sortBy (comparing fst) (G.map tag v))

-- | Transform a sort-by function into a sort-on function.
mkMutSortOn ::
  (PrimMonad m, G.Vector v a, G.Vector v (b, a), Ord b) =>
  -- | The sort-by sorting algorithm
  (((b, a) -> (b, a) -> Ordering) -> G.Mutable v (PrimState m) (b, a) -> m ()) ->
  -- | The sort-on function
  ((a -> b) -> G.Mutable v (PrimState m) a -> m ())
{-# INLINEABLE mkMutSortOn #-}
mkMutSortOn mutSortBy = \f mv ->
  let tag x = let y = f x in y `seq` (y, x)
   in do
        tagged <- G.freeze mv >>= G.thaw . G.map tag
        mutSortBy (comparing fst) tagged
        MG.iforM_ mv $ \i _ -> MG.read tagged i >>= MG.write mv i . snd

mkSortBy :: (G.Vector v a) => (forall s. (a -> a -> Ordering) -> G.Mutable v s a -> ST s ()) -> (a -> a -> Ordering) -> v a -> v a
{-# INLINEABLE mkSortBy #-}
mkSortBy mutSortBy cmp = \v -> runST $ do
  mv <- G.thaw v
  mutSortBy cmp mv
  G.freeze mv
