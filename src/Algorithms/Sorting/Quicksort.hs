{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- | Sort a vector via quicksort. This sorting algorithm is __unstable__.
--
-- There are a variety of pivot rules that can be used; use
-- 'quicksortWithPivot' or 'mutQuicksortWithPivot' to provide your
-- own. By default, a pivot rule is used that selects the median of
-- the first, middle, and last element of the slice being sorted. This
-- avoids worst-case behavior for sorted and reverse-sorted lists.
--
-- == Complexity
--
-- Quicksort runs in \(O(n^2)\) time in the worst case, and
-- \(O(n \log n)\) in the average case.
--
-- == References
--
-- This implementation follows
--
--   * Cormen, Thomas H and Leiserson, Charles E and Rivest, Ronald L
--     and Stein, Clifford, "Introduction to Algorithms", 3rd ed., pp.
--     171.
module Algorithms.Sorting.Quicksort
  ( -- * Immutable sorts
    quicksort,
    quicksortBy,
    quicksortOn,
    quicksortWithPivot,

    -- * Mutable sorts
    mutQuicksort,
    mutQuicksortBy,
    mutQuicksortOn,
    mutQuicksortWithPivot,

    -- * Partitioning vectors
    partition,
    mutPartition,

    -- * Pivot rules
    pivotMedianOf3,
    pivotLast,
  )
where

import Algorithms.Sorting.Sort3
import Algorithms.Sorting.Utility
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

type PivotRule m v a = G.Mutable v (PrimState m) a -> m a

quicksort :: (G.Vector v a, Ord a) => v a -> v a
quicksort = quicksortWithPivot (pivotMedianOf3 compare) compare

quicksortOn :: (G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> v a -> v a
quicksortOn = mkSortOn quicksortBy

quicksortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
quicksortBy by = quicksortWithPivot (pivotMedianOf3 by) by

quicksortWithPivot :: (G.Vector v a) => (forall s. PivotRule (ST s) v a) -> (a -> a -> Ordering) -> v a -> v a
quicksortWithPivot pivotRule cmp v = runST $ do
  v' <- G.thaw v
  mutQuicksortWithPivot pivotRule cmp v'
  G.freeze v'

mutQuicksort :: (PrimMonad m, G.Vector v a, Ord a) => G.Mutable v (PrimState m) a -> m ()
mutQuicksort = mutQuicksortBy compare

mutQuicksortOn :: (PrimMonad m, G.Vector v a, G.Vector v (b, a), Ord b) => (a -> b) -> G.Mutable v (PrimState m) a -> m ()
mutQuicksortOn = mkMutSortOn mutQuicksortBy

mutQuicksortBy :: (PrimMonad m, G.Vector v a) => (a -> a -> Ordering) -> G.Mutable v (PrimState m) a -> m ()
mutQuicksortBy by = mutQuicksortWithPivot (pivotMedianOf3 by) by

mutQuicksortWithPivot :: (PrimMonad m, G.Vector v a) => PivotRule m v a -> (a -> a -> Ordering) -> G.Mutable v (PrimState m) a -> m ()
mutQuicksortWithPivot pivotRule cmp v =
  let n = MG.length v
   in when (n > 0) $ do
        pivot <- pivotRule v
        (i, j) <- mutPartition cmp pivot v
        mutQuicksortWithPivot pivotRule cmp (MG.slice 0 i v)

        when (j < n) $
          mutQuicksortWithPivot pivotRule cmp (MG.slice j (n - j) v)

-- | Perform a 3-way partition of a vector around the given pivot.
--
-- This function returns two indices, @i@ and @j@, and a vector, @v@,
-- that satisfy the following:
--
-- 1. If @0 <= k && k < i@, then @v G.! k < pivot@
-- 2. If @i <= k && k < j@, then @v G.! k == pivot@
-- 3. If @j <= k@, then @v G.! k > pivot@
partition :: (G.Vector v a) => (a -> a -> Ordering) -> a -> v a -> (Int, Int, v a)
partition cmp pivot v = runST $ do
  mv <- G.thaw v
  (i, j) <- mutPartition cmp pivot mv
  v' <- G.freeze mv
  pure (i, j, v')

-- | Perform a 3-way partition of a vector in-place around the given pivot.
--
-- The returned indices satisfy the same properties as in 'partition',
-- but in the modified vector.
mutPartition ::
  (PrimMonad m, MG.MVector v a) => (a -> a -> Ordering) -> a -> v (PrimState m) a -> m (Int, Int)
mutPartition cmp pivot v =
  let n = MG.length v

      -- i <= k
      go i j k
        | k >= j = pure (i, j)
        | otherwise = do
            x <- MG.read v k
            case cmp x pivot of
              LT -> MG.swap v i k >> go (i + 1) j (k + 1)
              EQ -> go i j (k + 1)
              GT -> MG.swap v (j - 1) k >> go i (j - 1) k
   in do
        -- Partition elements, tracking where the pivots should end up
        go 0 n 0

-- | A pivot rule that selects the median of the first, middle, and
-- last element of an array.
--
-- This strategy avoids worst-case behavior when sorting ordered
-- arrays.
pivotMedianOf3 :: (PrimMonad m, MG.MVector v a) => (a -> a -> Ordering) -> v (PrimState m) a -> m a
pivotMedianOf3 cmp v =
  let n = MG.length v
      front = 0
      middle = n `div` 2
      back = n - 1
   in do
        x <- MG.read v front
        y <- MG.read v middle
        z <- MG.read v back

        let (_, median, _) = sort3By cmp x y z
        pure median

-- | A pivot rule that selects the final element of an array.
pivotLast :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> m a
pivotLast v = MG.read v (MG.length v - 1)
