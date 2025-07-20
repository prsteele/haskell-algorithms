module Algorithms.BinarySearch where

import Data.Vector.Generic qualified as G

-- | The leftmost index where the value can be inserted to maintain a sorted list.
--
-- If @i = bisectLeft v cmp x@, then
--
-- @
-- all ((== LT) . (`cmp` x)) (G.slice v 0 (i - 1)) == True
-- @
--
-- and
--
-- @
-- all ((== GE) . (`cmp` x)) (G.slice v i (G.length v - 1 - i)) == True
-- @
{-# INLINEABLE bisectLeft #-}
bisectLeft ::
  (G.Vector v a, Ord a) =>
  -- | The sorted vector. We do not verify that @v@ is sorted.
  v a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  Int
bisectLeft = bisectLeftBy compare

-- | The leftmost index where the value can be inserted to maintain a sorted list.
--
-- If @i = bisectLeft v cmp x@, then
--
-- @
-- all ((== LT) . (`cmp` x)) (G.slice v 0 (i - 1)) == True
-- @
--
-- and
--
-- @
-- all ((== GE) . (`cmp` x)) (G.slice v i (G.length v - 1 - i)) == True
-- @
{-# INLINEABLE bisectLeftBy #-}
bisectLeftBy ::
  (G.Vector v a) =>
  -- | The ordering function
  (a -> a -> Ordering) ->
  -- | The sorted vector. We do not verify that @v@ is sorted.
  v a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  Int
bisectLeftBy cmp v x = go 0 (G.length v)
  where
    go l u
      | l == u = l
      | otherwise =
          let m = l + (u - l) `quot` 2
           in case cmp (v G.! m) x of
                LT -> go (m + 1) u
                _ -> go l m

-- | The rightmost index where the value can be inserted to maintain a sorted list.
--
-- If @i = bisectRight v cmp x@, then
--
-- @
-- all ((== LE) . (`cmp` x)) (G.slice v 0 i) == True
-- @
--
-- and
--
-- @
-- all ((== GT) . (`cmp` x)) (G.slice v (i + 1) (G.length v - i)) == True
-- @
{-# INLINEABLE bisectRight #-}
bisectRight ::
  (G.Vector v a, Ord a) =>
  -- | The sorted vector. We do not verify that @v@ is sorted.
  v a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  Int
bisectRight = bisectRightBy compare

-- | The rightmost index where the value can be inserted to maintain a sorted list.
--
-- If @i = bisectRight v cmp x@, then
--
-- @
-- all ((== LE) . (`cmp` x)) (G.slice v 0 i) == True
-- @
--
-- and
--
-- @
-- all ((== GT) . (`cmp` x)) (G.slice v (i + 1) (G.length v - i)) == True
-- @
{-# INLINEABLE bisectRightBy #-}
bisectRightBy ::
  (G.Vector v a) =>
  -- | The ordering function
  (a -> a -> Ordering) ->
  -- | The sorted vector. We do not verify that @v@ is sorted.
  v a ->
  -- | The element to be inserted
  a ->
  -- | The insertion index.
  Int
bisectRightBy cmp v x = go 0 (G.length v)
  where
    go l u
      | l == u = l
      | otherwise =
          let m = l + (u - l) `quot` 2
           in case cmp (v G.! m) x of
                GT -> go l m
                _ -> go (m + 1) u

-- | The leftmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findLeft #-}
findLeft :: (G.Vector v a, Ord a) => v a -> a -> Maybe Int
findLeft = findLeftBy compare

-- | The leftmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findLeftBy #-}
findLeftBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> a -> Maybe Int
findLeftBy cmp v x =
  let ix = bisectLeftBy cmp v x
   in case v G.!? ix of
        Nothing -> Nothing
        Just y ->
          if EQ == cmp x y
            then Just ix
            else Nothing

-- | The rightmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findRight #-}
findRight :: (G.Vector v a, Ord a) => v a -> a -> Maybe Int
findRight = findRightBy compare

-- | The rightmost index of an element in a sorted list, if it exists.
{-# INLINEABLE findRightBy #-}
findRightBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> a -> Maybe Int
findRightBy cmp v x =
  let ix = bisectRightBy cmp v x - 1
   in case v G.!? ix of
        Nothing -> Nothing
        Just y ->
          if EQ == cmp x y
            then Just ix
            else Nothing
