-- | Sort a vector with heap sort. This sorting algorithm is __unstable__.
--
-- The transformation performed by 'heapsortOn' and 'mutHeapsortOn' is
-- performed exactly once per element.
--
-- == Complexity
--
-- This algorithm runs in \(O(n \log n)\) time, where \(n\) is the
-- length of the input.
--
-- == References
--
-- This implementation follows
--
--   * Cormen, Thomas H and Leiserson, Charles E and Rivest, Ronald L
--     and Stein, Clifford, "Introduction to Algorithms", 3rd ed., pp.
--     160.
module Algorithms.Sorting.HeapSort
  ( -- * Immutable sorts
    heapsort,
    heapsortBy,
    heapsortOn,

    -- * Mutable sorts
    mutHeapsort,
    mutHeapsortBy,
    mutHeapsortOn,
  )
where

import Algorithms.Sorting.Utility
import Algorithms.Utility
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Heap.Fixed as FH
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

heapsort :: (Ord a, G.Vector v a) => v a -> v a
heapsort = heapsortBy compare

heapsortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
heapsortBy = mkSortBy mutHeapsortBy

heapsortOn :: (G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> v a -> v a
heapsortOn = mkSortOn heapsortBy

mutHeapsort :: (Ord a, PrimMonad m, G.Vector v a) => G.Mutable v (PrimState m) a -> m ()
mutHeapsort = mutHeapsortBy compare

mutHeapsortBy :: (PrimMonad m, G.Vector v a) => (a -> a -> Ordering) -> G.Mutable v (PrimState m) a -> m ()
mutHeapsortBy cmp mv = do
  -- Create the initial heap
  _ <- FH.heap cmp mv

  forM_ ((MG.length mv - 1) `downTo` 1) $ \i -> do
    -- Put the maximum element at the end
    MG.swap mv 0 i

    -- Restore the heap property, exluding the already-sorted elements.
    FH.heapify (FH.Heap (cmp, MG.slice 0 i mv)) 0

mutHeapsortOn :: (PrimMonad m, G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> G.Mutable v (PrimState m) a -> m ()
mutHeapsortOn = mkMutSortOn mutHeapsortBy
