module Algorithms.Sorting.CountingSort where

import Algorithms.Utility
import Control.Monad
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Sort a vector whose elements are integer-valued in a finite range.
--
-- Counting sort is a non-comparison based sort linear-time algorithm
-- for sorting lists of integers, or objects that can be put in
-- correspondence with the integers.
--
-- If an element is found that lies outside the stated range, a 'Left'
-- is returned with that value.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | No         |
-- +------------+------------+
-- | Stable     | Yes        |
-- +------------+------------+
--
-- == Complexity
--
-- This algorithm has complexity \(O(n k)\), where \(n\) is the length of
-- the vector and \(k\) is the difference between the minimum and
-- maximum values to be sorted.
--
-- == References
--
-- This implementation follows
--
--   * Cormen, Thomas H and Leiserson, Charles E and Rivest, Ronald L
--     and Stein, Clifford, "Introduction to Algorithms", 3rd ed., pp.
--     195.
countingSort :: (G.Vector v a, Integral a) => (a, a) -> v a -> Either a (v a)
countingSort (lo, hi) v = runST (runExceptT (countingSortST (lo, hi) v))

-- | Counting sort, in the ST monad.
countingSortST :: (G.Vector v a, Integral a) => (a, a) -> v a -> ExceptT a (ST s) (v a)
countingSortST (lo, hi) v =
  let n :: Int
      n = G.length v

      toIx val
        | val < lo || hi < val = throwError val
        | otherwise = pure . fromIntegral $ val - lo
   in do
        -- Count the occurrences of each value
        counts <- getCounts (lo, hi) v

        -- Modify counts to contain the cumulative sum
        cumSum counts

        -- Create and populate the result vector
        result <- MG.new n
        forM_ ((n - 1) `downTo` 0) $ \i -> do
          let entry = v G.! i
          ix <- toIx entry
          count <- UM.read counts ix

          let resultIx = count - 1

          MG.write result resultIx entry
          MG.write counts ix (count - 1)

        G.freeze result

-- | Count the occurrences of each element of the input vector.
getCounts ::
  (Integral a, G.Vector v a, PrimMonad m) =>
  (a, a) ->
  v a ->
  ExceptT a m (UM.MVector (PrimState m) Int)
getCounts (lo, hi) v =
  let n :: Int
      n = G.length v

      k :: Int
      k = fromIntegral (hi - lo + 1)

      toIx val
        | val < lo || hi < val = throwError val
        | otherwise = pure . fromIntegral $ val - lo
   in do
        -- Count the occurrences of each value
        counts <- UM.replicate k (0 :: Int)
        forM_ (0 `upTo` (n - 1)) $ \i -> do
          let key = v G.! i
          ix <- toIx key
          count <- UM.read counts ix
          UM.write counts ix (count + 1)

        pure counts

-- | Perform an in-place cumulative sum
cumSum :: (Num a, MG.MVector v a, PrimMonad m) => v (PrimState m) a -> m ()
cumSum v =
  let n = MG.length v
   in forM_ (1 `upTo` (n - 1)) $ \i -> do
        prev <- MG.read v (i - 1)
        cur <- MG.read v i
        MG.write v i (cur + prev)
