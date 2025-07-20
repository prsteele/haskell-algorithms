module Algorithms.BinarySearch.Mutable where

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable qualified as MG

-- | The leftmost index where the value can be inserted to maintain a sorted list.
--
-- This is identical to 'Algorithms.BinarySearch.bisectLeft' but for mutable vectors.
{-# INLINEABLE bisectLeft #-}
bisectLeft ::
  (PrimMonad m, s ~ PrimState m, MG.MVector mv a, Ord a) =>
  -- | The sorted vector. We do not verify that @mv@ is sorted.
  mv s a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  m Int
bisectLeft = bisectLeftBy compare

-- | The leftmost index where the value can be inserted to maintain a sorted list.
--
-- This is identical to 'Algorithms.BinarySearch.bisectLeftBy' but for mutable vectors.
{-# INLINEABLE bisectLeftBy #-}
bisectLeftBy ::
  (PrimMonad m, s ~ PrimState m, MG.MVector mv a) =>
  -- | The ordering function
  (a -> a -> Ordering) ->
  -- | The sorted vector. We do not verify that @v@ is sorted.
  mv s a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  m Int
bisectLeftBy cmp mv x = go 0 (MG.length mv)
  where
    go l u
      | l == u = pure l
      | otherwise =
          let m = l + (u - l) `quot` 2
           in do
                y <- MG.read mv m
                case cmp y x of
                  LT -> go (m + 1) u
                  _ -> go l m

-- | The rightmost index where the value can be inserted to maintain a sorted list.
--
-- This is identical to 'Algorithms.BinarySearch.bisectRight' but for mutable vectors.
{-# INLINEABLE bisectRight #-}
bisectRight ::
  (PrimMonad m, s ~ PrimState m, MG.MVector mv a, Ord a) =>
  -- | The sorted vector. We do not verify that @v@ is sorted.
  mv s a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  m Int
bisectRight = bisectRightBy compare

-- | The rightmost index where the value can be inserted to maintain a sorted list.
--
-- This is identical to 'Algorithms.BinarySearch.bisectRightBy' but for mutable vectors.
{-# INLINEABLE bisectRightBy #-}
bisectRightBy ::
  (PrimMonad m, s ~ PrimState m, MG.MVector mv a) =>
  -- | The ordering function
  (a -> a -> Ordering) ->
  -- | The sorted vector. We do not verify that @v@ is sorted.
  mv s a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  m Int
bisectRightBy cmp mv x = go 0 (MG.length mv)
  where
    go l u
      | l == u = pure l
      | otherwise =
          let m = l + (u - l) `quot` 2
           in do
                y <- MG.read mv m
                case cmp y x of
                  GT -> go l m
                  _ -> go (m + 1) u

-- | The leftmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findLeft #-}
findLeft :: (PrimMonad m, s ~ PrimState m, MG.MVector mv a, Ord a) => mv s a -> a -> m (Maybe Int)
findLeft = findLeftBy compare

-- | The leftmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findLeftBy #-}
findLeftBy :: (PrimMonad m, s ~ PrimState m, MG.MVector mv a) => (a -> a -> Ordering) -> mv s a -> a -> m (Maybe Int)
findLeftBy cmp mv x = do
  ix <- bisectLeftBy cmp mv x
  my <- MG.readMaybe mv ix
  pure $ case my of
    Nothing -> Nothing
    Just y ->
      if EQ == cmp x y
        then Just ix
        else Nothing

-- | The rightmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findRight #-}
findRight :: (PrimMonad m, s ~ PrimState m, MG.MVector mv a, Ord a) => mv s a -> a -> m (Maybe Int)
findRight = findRightBy compare

-- | The rightmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findRightBy #-}
findRightBy :: (PrimMonad m, s ~ PrimState m, MG.MVector mv a) => (a -> a -> Ordering) -> mv s a -> a -> m (Maybe Int)
findRightBy cmp mv x = do
  ix' <- bisectRightBy cmp mv x
  let ix = ix' - 1
  my <- MG.readMaybe mv ix
  pure $ case my of
    Nothing -> Nothing
    Just y ->
      if EQ == cmp x y
        then Just ix
        else Nothing
