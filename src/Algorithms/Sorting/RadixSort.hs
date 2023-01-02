{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Sort a vector with radix sort. This sorting algorithm is __stable__.
--
-- This is a specialized non-comparison based linear-time sorting algorithm.
--
-- == Complexity
--
-- This algorithm runs in \(O(n d)\) time, where @n@ is the length of
-- the array and @d@ is the number of radices we sort on. (This
-- assumes that comparing two radices is a constant-time operation.)
module Algorithms.Sorting.RadixSort
  ( -- * Immutable sorts
    radixSort,
    twosComplementRadixSort,

    -- * Mutable sorts
    mutRadixSort,
    mutTwosComplementRadixSort,

    -- * Helper functions
    twosComplementRadix,

    -- * Examples
    exampleTupleRadixSort,
  )
where

import Algorithms.Sequence
import Algorithms.Sorting.MergeSort (mutMergeSortOn)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Sort a vector via radix sort.
radixSort ::
  G.Vector v a =>
  -- | The radix function
  (a -> Int -> b) ->
  -- | The in-place, stable sorting subroutine
  (forall s. (a -> b) -> G.Mutable v (PrimState (ST s)) a -> ST s ()) ->
  -- | The number of radices to sort on
  Int ->
  -- | The vector to sort
  v a ->
  -- | The sorted vector
  v a
radixSort radix sortOn d v = runST $ do
  mv <- G.thaw v
  mutRadixSort radix sortOn d mv
  G.freeze mv

-- | A version of 'radixSort' specialized for types implementing 'FiniteBits'.
twosComplementRadixSort :: forall a v. (FiniteBits a, G.Vector v a, G.Vector v (Bool, a)) => v a -> v a
twosComplementRadixSort = radixSort twosComplementRadix mutMergeSortOn (finiteBitSize (undefined :: a))

-- | A version of 'radixSort' specialized for types implementing 'FiniteBits'.
mutTwosComplementRadixSort ::
  forall m a v.
  (PrimMonad m, G.Vector v a, G.Vector v (Bool, a), FiniteBits a) =>
  G.Mutable v (PrimState m) a ->
  m ()
mutTwosComplementRadixSort = mutRadixSort twosComplementRadix mutMergeSortOn (finiteBitSize (undefined :: a))

-- | A radix function suitable for sorting two's complement encoded
-- integers.
--
-- We can almost use 'testBit' directly, but we need to negate the
-- sign bit.
twosComplementRadix :: FiniteBits a => a -> Int -> Bool
twosComplementRadix x i
  | i == bits - 1 = not (testBit x i)
  | otherwise = testBit x i
  where
    bits = finiteBitSize (undefined `asTypeOf` x)

mutRadixSort ::
  (PrimMonad m, MG.MVector v a) =>
  -- | The radix function
  (a -> Int -> b) ->
  -- | The in-place, stable sorting subroutine
  ((a -> b) -> v (PrimState m) a -> m ()) ->
  -- | The number of radices to sort on
  Int ->
  -- | The vector to sort
  v (PrimState m) a ->
  m ()
mutRadixSort radix sortOn d mv = do
  forM_ (0 `upTo` (d - 1)) $ \i ->
    sortOn (`radix` i) mv

-- | A sample use of 'radixSort' for sorting triples of integers
exampleTupleRadixSort :: (G.Vector v (Int, Int, Int), G.Vector v (Int, (Int, Int, Int))) => v (Int, Int, Int) -> v (Int, Int, Int)
exampleTupleRadixSort = radixSort f mutMergeSortOn 3
  where
    f (x, _, _) 2 = x
    f (_, x, _) 1 = x
    f (_, _, x) 0 = x
    f _ _ = 0 -- Won't be called
