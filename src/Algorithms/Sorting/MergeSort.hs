{-# LANGUAGE ScopedTypeVariables #-}

-- | Sort a vector with merge sort.
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
module Algorithms.Sorting.MergeSort where

import Algorithms.Utility
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Sort a vector.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | No         |
-- +------------+------------+
-- | Stable     | Yes        |
-- +------------+------------+
mergeSort :: (G.Vector v a, Ord a) => v a -> v a
mergeSort v = runST $ do
  mv <- G.thaw v
  mutMergeSort mv
  G.freeze mv

-- | Sort a vector in-place.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | Yes        |
-- +------------+------------+
-- | Stable     | Yes        |
-- +------------+------------+
mutMergeSort :: (PrimMonad m, G.Vector v a, Ord a) => G.Mutable v (PrimState m) a -> m ()
mutMergeSort mv
  | MG.length mv <= 1 = pure ()
  | otherwise =
    let n = MG.length mv
        mid = n `div` 2

        left = MG.slice 0 mid mv
        right = MG.slice mid (n - mid) mv
     in do
          -- Recurse left
          mutMergeSort left

          -- Recurse right
          mutMergeSort right

          -- Merge
          left' <- G.freeze left
          right' <- G.freeze right
          mutMerge left' right' mv

merge :: (G.Vector v a, Ord a) => v a -> v a -> v a
merge u v = runST $ do
  w <- MG.new (G.length u + G.length v)
  mutMerge u v w
  G.freeze w

-- | Merge two vectors into a third, in-place.
--
-- The two input vectors are assumed to be sorted, and it is further
-- assumed that the length of the two input vectors sum to the length
-- of the output vector. Neither of these preconditions are checked.
mutMerge ::
  (PrimMonad m, G.Vector v a, MG.MVector mv a, Ord a) =>
  v a ->
  v a ->
  mv (PrimState m) a ->
  m ()
mutMerge left right output =
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

          if l <= r
            then MG.write output k l >> go (i + 1) j (k + 1)
            else MG.write output k r >> go i (j + 1) (k + 1)
   in go 0 0 0
