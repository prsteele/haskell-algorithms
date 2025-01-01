{-# LANGUAGE ConstraintKinds #-}

module Algorithms.MaximumSubArray where

import Data.Function (on)
import Data.Semigroup
import qualified Data.Vector.Generic as G
import GHC.Generics
import Optics

-- | The half-closed indices defining the slice of a vector.
data Slice = Slice
  { lo :: !Int,
    hi :: !Int
  }
  deriving (Show, Generic)

data SubArraySum a = SubArraySum
  { slice :: !Slice,
    value :: !a
  }
  deriving (Show, Generic)

instance (Eq a) => Eq (SubArraySum a) where
  (==) = (==) `on` (^. #value)

instance (Ord a) => Ord (SubArraySum a) where
  compare = compare `on` (^. #value)

-- | A convenience constraint describing orderable Monoids (like @Int@.)
type Num' a = (Ord a, Monoid a)

-- | Find the maximum slice over the given vector.
--
-- Unless the input vector is empty, empty slices are not permitted.
-- If you wish to allow empty slices, then you can just inspect the
-- resulting value and compare it to your zero value.
--
-- For the common case of summing real numbers, see 'mMaximumSubArraySum'.
maximumSubArray :: (Num' a, G.Vector v a) => v a -> SubArraySum a
maximumSubArray v = maximumSubArray' v (Slice 0 (G.length v))

-- | A specialization of 'findMaximumSubArray' for numbers.
maximumSubArrayNum :: (Num a, Ord a, G.Vector v a, G.Vector v (Sum a)) => v a -> SubArraySum a
maximumSubArrayNum = (#value %~ getSum) . maximumSubArray . G.map Sum

maximumSubArray' :: (Num' a, G.Vector v a) => v a -> Slice -> SubArraySum a
maximumSubArray' v ixs@(Slice l r)
  -- Base case; should only occur when the input array is empty
  | l == r = SubArraySum ixs mempty
  -- Base case
  | l + 1 == r = SubArraySum ixs (v G.! l)
  -- Recursive case; there are at least two elements in the range, so
  -- the left, right, and crossing cases are all nonempty as well.
  | otherwise =
      let mid = (l + r) `div` 2
          left = maximumSubArray' v (ixs & #hi .~ mid)
          right = maximumSubArray' v (ixs & #lo .~ mid)
          crossing = findMaxCrossingSubArray v l mid r
       in maximum [left, crossing, right]

-- | Find the maximum subarray crossing the given index.
--
-- If @findMaxCrossingSubArray v i@ returns the @Slice (x, y)@, then @x <= i && i < y@.
--
-- Preconditions: In @findMaxCrossingSubArray v l m r@, we need
--
-- * @0 <= l@
-- * @l <= m@
-- * @m < r@
-- * @r <= G.length v@
findMaxCrossingSubArray :: (Num' a, G.Vector v a) => v a -> Int -> Int -> Int -> SubArraySum a
findMaxCrossingSubArray v l m r =
  let -- Slices from the left, including the midpoint
      lefts =
        take ((m + 1) - l)
          . iterate (growLeft v)
          $ SubArraySum (Slice m (m + 1)) (v G.! m)

      -- Slices from the right, excluding the midpoint.
      rights =
        take (r - m)
          . iterate (growRight v)
          $ SubArraySum (Slice (m + 1) (m + 1)) mempty

      (SubArraySum (Slice l' _) lx) = maximum lefts
      (SubArraySum (Slice _ r') rx) = maximum rights
   in SubArraySum (Slice l' r') (lx <> rx)

growRight :: (Num' a, G.Vector v a) => v a -> SubArraySum a -> SubArraySum a
growRight v (SubArraySum (Slice l r) x) = SubArraySum (Slice l (r + 1)) (x <> v G.! r)

growLeft :: (Num' a, G.Vector v a) => v a -> SubArraySum a -> SubArraySum a
growLeft v (SubArraySum (Slice l r) x) = SubArraySum (Slice (l - 1) r) (x <> v G.! (l - 1))
