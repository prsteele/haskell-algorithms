-- |
--
-- Insertion sort for Vectors. This algorithm has complexity @O(n^2)@
-- for vectors of length @n@, so use with care.
--
-- = References
--
-- This implementation follows
--
--   * Cormen, Thomas H and Leiserson, Charles E and Rivest, Ronald L
--     and Stein, Clifford, "Introduction to Algorithms", 3rd ed., pp.
--     18.
module Algorithms.Sorting.InsertionSort where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Sort an immutable vector via insertion sort.
--
-- If you already have a mutable copy of your vector, you can use
-- 'mutInsertionSort'.
insertionSort :: (G.Vector v a, Ord a) => v a -> v a
insertionSort v = runST $ do
  mv <- G.thaw v
  mutInsertionSort mv
  G.freeze mv

-- | Sort a mutable vector via insertion sort.
mutInsertionSort :: (PrimMonad m, MG.MVector v a, Ord a) => v (PrimState m) a -> m ()
mutInsertionSort v =
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
          if key < x
            then MG.write v (i + 1) x >> shift key (i - 1)
            else pure (i + 1)
   in forM_ [1 .. n - 1] insert
