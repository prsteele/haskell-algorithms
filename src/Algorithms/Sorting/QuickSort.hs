{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Algorithms.Sorting.QuickSort where

import Algorithms.Sorting.Sort3
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Sort a vector via quicksort.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | No         |
-- +------------+------------+
-- | Stable     | No         |
-- +------------+------------+
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
quickSort :: (G.Vector v a, Ord a) => v a -> v a
quickSort = quickSortWithPivot pivotMedianOf3

mutQuicksort :: (PrimMonad m, MG.MVector v a, Ord a) => v (PrimState m) a -> m ()
mutQuicksort = mutQuicksortWithPivot pivotMedianOf3

type PivotRule v a = (forall s. G.Mutable v (PrimState (ST s)) a -> ST s Int)

quickSortWithPivot :: (G.Vector v a, Ord a) => PivotRule v a -> v a -> v a
quickSortWithPivot pivotRule v = runST $ do
  v' <- G.thaw v
  mutQuicksortWithPivot pivotRule v'
  G.freeze v'

mutQuicksortWithPivot :: (PrimMonad m, MG.MVector v a, Ord a) => (v (PrimState m) a -> m Int) -> v (PrimState m) a -> m ()
mutQuicksortWithPivot pivotRule v =
  let n = MG.length v
   in when (n > 0) $ do
        i <- partition pivotRule v
        mutQuicksortWithPivot pivotRule (MG.slice 0 i v)
        mutQuicksortWithPivot pivotRule (MG.slice (i + 1) (n - (i + 1)) v)

-- | Partition a non-empty vector using a the given pivot rule.
--
-- Whether the vector is non-empty is not checked.
partition ::
  (PrimMonad m, MG.MVector v a, Ord a) => (v (PrimState m) a -> m Int) -> v (PrimState m) a -> m Int
partition pivotRule v =
  let n = MG.length v

      swap i j = do
        ix <- MG.read v i
        jx <- MG.read v j
        MG.write v i jx
        MG.write v j ix

      go pivot i j
        | j == n - 1 = pure i
        | otherwise = do
          x <- MG.read v j
          let doSwap = x <= pivot
          when doSwap (swap (i + 1) j)
          go pivot (if doSwap then i + 1 else i) (j + 1)
   in do
        -- Swap the selected pivot to the end of the array
        let pivotIx = n - 1
        pivotRule v >>= swap pivotIx
        pivot <- MG.read v pivotIx

        -- Partition elements, tracking where the pivot should end up
        i <- fmap (+ 1) (go pivot (-1) 0)

        -- Send the pivot to the proper index
        swap i pivotIx
        pure i

pivotLast :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> m Int
pivotLast v = pure (MG.length v - 1)

pivotMedianOf3 :: (PrimMonad m, MG.MVector v a, Ord a) => v (PrimState m) a -> m Int
pivotMedianOf3 v =
  let n = MG.length v
      front = 0
      middle = n `div` 2
      back = n - 1

      readIx i = fmap (,i) (MG.read v i)
   in do
        x <- readIx front
        y <- readIx middle
        z <- readIx back

        let (_, (_, median), _) = sort3 x y z
        pure median
