-- | Sort a vector with merge sort. This sorting algorithm is __stable__.
--
-- The transformation performed by 'mergeSortOn' and 'mutMergeSortOn'
-- is performed exactly once per element.
--
-- == Complexity
--
-- This algorithm runs in \(O(n \log n)\) time, where \(n\) is the
-- length of the input. It uses \(\Theta(n)\) additional space, even
-- when run in place.
--
-- == References
--
-- This implementation follows
--
--   * Cormen, Thomas H and Leiserson, Charles E and Rivest, Ronald L
--     and Stein, Clifford, "Introduction to Algorithms", 3rd ed., pp.
--     31.
module Algorithms.Sorting.MergeSort
  ( -- * Immutable sorts
    mergeSort,
    mergeSortBy,
    mergeSortOn,

    -- * Mutable sorts
    mutMergeSort,
    mutMergeSortBy,
    mutMergeSortOn,

    -- * Merging vectors
    merge,
    mutMerge,
  )
where

import Algorithms.Sorting.Utility
import Algorithms.Utility
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

mergeSort :: (G.Vector v a, Ord a) => v a -> v a
mergeSort = mergeSortBy compare

mergeSortOn :: (G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> v a -> v a
mergeSortOn = mkSortOn mergeSortBy

mergeSortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
mergeSortBy = mkSortBy mutMergeSortBy

mutMergeSort :: (PrimMonad m, G.Vector v a, Ord a) => G.Mutable v (PrimState m) a -> m ()
mutMergeSort = mutMergeSortBy compare

mutMergeSortOn :: (PrimMonad m, G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> G.Mutable v (PrimState m) a -> m ()
mutMergeSortOn = mkMutSortOn mutMergeSortBy

mutMergeSortBy :: (PrimMonad m, G.Vector v a) => (a -> a -> Ordering) -> G.Mutable v (PrimState m) a -> m ()
mutMergeSortBy cmp mv
  | MG.length mv <= 1 = pure ()
  | otherwise =
      let n = MG.length mv
          mid = n `div` 2

          left = MG.slice 0 mid mv
          right = MG.slice mid (n - mid) mv
       in do
            -- Recurse left
            mutMergeSortBy cmp left

            -- Recurse right
            mutMergeSortBy cmp right

            -- Merge
            left' <- G.freeze left
            right' <- G.freeze right
            mutMerge cmp left' right' mv

-- | Merge two sorted vectors into a new sorted vector.
merge :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a -> v a
merge cmp u v = runST $ do
  w <- MG.new (G.length u + G.length v)
  mutMerge cmp u v w
  G.freeze w

-- | Merge two vectors into a third, in-place.
--
-- The two input vectors are assumed to be sorted, and it is further
-- assumed that the length of the two input vectors sum to the length
-- of the output vector. Neither of these preconditions are checked.
mutMerge ::
  (PrimMonad m, G.Vector v a, MG.MVector mv a) =>
  (a -> a -> Ordering) ->
  v a ->
  v a ->
  mv (PrimState m) a ->
  m ()
mutMerge cmp left right output =
  let lenL = G.length left
      lenR = G.length right

      go i j k
        -- Left and right are done, halt
        | k == lenL + lenR = pure ()
        -- Left is done, finish with right
        | i == lenL = assignSlice (lenL + j) (G.slice j (lenR - j) right) output
        -- Right is done, finish with left
        | j == lenR = assignSlice (lenR + i) (G.slice i (lenL - i) left) output
        -- Both left and right have remaining elements
        | otherwise = do
            let l = left G.! i
                r = right G.! j

            if cmp l r /= GT
              then MG.write output k l >> go (i + 1) j (k + 1)
              else MG.write output k r >> go i (j + 1) (k + 1)
   in go 0 0 0
