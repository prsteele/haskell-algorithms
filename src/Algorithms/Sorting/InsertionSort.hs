{-# LANGUAGE FlexibleContexts #-}

-- | Sort a vector via insertion sort. This sorting algorithm is __stable__.
--
-- This can be very quick on small arrays, but has very poor
-- performance in general.
--
-- The transformation performed by 'insertionSortOn' and
-- 'mutInsertionSortOn' is performed exactly once per element.
--
-- == Complexity
--
-- This algorithm has complexity \(O(n^2)\), where \(n\) is the length of
-- the vector. However, it has complexity \(O(n)\) on sorted lists.
--
-- == References
--
-- This implementation follows
--
--   * Cormen, Thomas H and Leiserson, Charles E and Rivest, Ronald L
--     and Stein, Clifford, "Introduction to Algorithms", 3rd ed., pp.
--     18.
module Algorithms.Sorting.InsertionSort
  ( -- * Immutable sorts
    insertionSort,
    insertionSortBy,
    insertionSortOn,

    -- * Mutable sorts
    mutInsertionSort,
    mutInsertionSortBy,
    mutInsertionSortOn,
  )
where

import Algorithms.Sorting.Utility
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

insertionSort :: (G.Vector v a, Ord a) => v a -> v a
insertionSort = insertionSortBy compare

insertionSortBy :: G.Vector v a => (a -> a -> Ordering) -> v a -> v a
insertionSortBy cmp v = runST $ do
  mv <- G.thaw v
  mutInsertionSortBy cmp mv
  G.freeze mv

insertionSortOn :: (G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> v a -> v a
insertionSortOn = mkSortOn insertionSortBy

mutInsertionSort :: (PrimMonad m, MG.MVector v a, Ord a) => v (PrimState m) a -> m ()
mutInsertionSort = mutInsertionSortBy compare

mutInsertionSortBy :: (PrimMonad m, MG.MVector v a) => (a -> a -> Ordering) -> v (PrimState m) a -> m ()
mutInsertionSortBy cmp v =
  let n = MG.length v

      -- Insert the ith element into the first i-1 sorted elements.
      -- Note that i > 0.
      insert i = do
        -- Find the key to insert
        key <- MG.read v i

        -- Clear a space for the key
        j <- shift key (i - 1)

        -- Insert the key
        MG.write v j key

      -- Shift elements until a spot is found for the key.
      shift key i
        | i < 0 = pure 0
        | otherwise = do
          x <- MG.read v i
          if cmp key x == LT
            then MG.write v (i + 1) x >> shift key (i - 1)
            else pure (i + 1)
   in forM_ [1 .. n - 1] insert

mutInsertionSortOn :: (PrimMonad m, G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> G.Mutable v (PrimState m) a -> m ()
mutInsertionSortOn = mkMutSortOn mutInsertionSortBy
