{-# LANGUAGE TypeOperators #-}

module Data.Heap.Generic
  ( -- * Heaps
    FixedHeap (..),
    Heap (..),

    -- * Intrusive heaps
    IntrusiveFixedHeap (..),
    IntrusiveHeap (..),
    Reprioritize,

    -- ** Fixed-size heap operations
    build,
    heapify,
    peek,
    peek',
    popPush,
    popPush',
    pushPop,
    pushPop',
    reprioritize,
    size,

    -- ** Size-changing heap operations
    clear,
    push,
    push',
    pop,

    -- * Internal helper functions
    FixedHeapable (..),
    Heapable (..),
    genericBuild,
    genericHeapify,
    genericHandleToCallback,
    genericHeapPopPush,
    genericHeapPopPush',
    genericHeapPushPop,
    genericHeapPushPop',
    genericHeapReprioritize,
    genericHeapReprioritize',
    genericHeapPriority',
    genericHeapPop,
    genericHeapPush,
    genericHeapPush',
    genericHeapSize,
    genericPeek,
    genericPeek',
    left,
    right,
    parent,
  )
where

import Algorithms.Utility
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Function
import Data.Kind
import Data.Maybe

class FixedHeapable f a where
  type Handle f s :: Type
  basicHeapCompare :: f s a -> a -> a -> Ordering
  basicHeapRead :: f s a -> Int -> ST s (a, Handle f s)
  basicHeapReadMaybe :: f s a -> Int -> ST s (Maybe (a, Handle f s))
  basicHeapWrite :: f s a -> Int -> a -> Handle f s -> ST s ()
  basicHeapLength :: f s a -> ST s Int
  basicHeapMakeHandle :: f s a -> ST s (Handle f s)
  basicHeapExpireHandle :: f s a -> Handle f s -> ST s ()
  basicHeapReadHandle :: f s a -> Handle f s -> ST s (Maybe (Int, a))

class (FixedHeapable f a) => Heapable f a where
  basicHeapAppend :: f s a -> a -> Handle f s -> ST s ()
  basicHeapShrink :: f s a -> ST s ()

-- | Fixed-size max heaps.
class FixedHeap f a where
  -- | Enforce the heap property from an arbitrary initial state.
  basicHeapBuild :: f s a -> ST s ()

  -- | Enforce the heap property for a particular index.
  basicHeapify :: f s a -> Int -> ST s ()

  -- | Peek at the maximum element on the heap, but do not alter the heap.
  basicHeapPeek :: f s a -> ST s (Maybe a)

  -- | Pop an element from the heap, and then push a new one on.
  --
  -- The pushed element cannot be returned.
  --
  -- If the heap is non-empty, a 'Just' will be returned.
  basicHeapPopPush :: f s a -> a -> ST s (Maybe a)

  -- | Push an element onto the heap, and then pop the maximum element off.
  --
  -- The pushed element will be considered when finding the maximum element.
  basicHeapPushPop :: f s a -> a -> ST s a

  -- | Alter the priority of a heap element.
  --
  -- If the new priority is larger than the old priority, this should
  -- run in \(O(\log n)\) time, where \(n\) is the size of the heap;
  -- otherwise, it will take \(O(n \log n)\) time
  basicHeapReprioritize :: f s a -> Int -> a -> ST s ()

  -- | The number of elements in the heap.
  basicHeapSize :: f s a -> ST s Int

-- | Fixed-size max heaps with pointers to heap elements.
--
-- The pointers can be used to
--
-- 1. Identify the location of heap elements in the underlying storage
--
-- 2. Modify the priority of heap elements while maintaining the heap
--    invariant.
class (FixedHeap f a) => IntrusiveFixedHeap f a where
  basicHeapPeek' :: f s a -> ST s (Maybe (a, Reprioritize (ST s) a))
  basicHeapPopPush' :: f s a -> a -> ST s (Maybe (a, Reprioritize (ST s) a))
  basicHeapPushPop' :: f s a -> a -> ST s (Reprioritize (ST s) a, a)

-- | A dynamically-sized max heap.
class (FixedHeap f a) => Heap f a where
  basicClear :: f s a -> ST s ()
  basicHeapPush :: f s a -> a -> ST s ()
  basicHeapPop :: f s a -> ST s (Maybe a)

-- | A dynamically-sized max heap with pointers to heap elements.
class (FixedHeap f a, IntrusiveFixedHeap f a, Heap f a) => IntrusiveHeap f a where
  basicHeapPush' :: f s a -> a -> ST s (Reprioritize (ST s) a)

type Reprioritize m a = a -> m ()

genericHandleToCallback :: (FixedHeapable f a) => f s a -> Handle f s -> Reprioritize (ST s) a
genericHandleToCallback h ref x = do
  mx <- basicHeapReadHandle h ref
  case mx of
    Nothing -> pure ()
    Just (ix, _) -> basicHeapWrite h ix x ref

build :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> m ()
build = stToPrim . basicHeapBuild

heapify :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> Int -> m ()
heapify h = stToPrim . basicHeapify h

peek :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> m (Maybe a)
peek = stToPrim . basicHeapPeek

peek' :: (PrimMonad m, IntrusiveFixedHeap f a, s ~ PrimState m) => f s a -> m (Maybe (a, Reprioritize m a))
peek' h = stToPrim $ do
  mx <- basicHeapPeek' h
  case mx of
    Nothing -> pure Nothing
    Just (x, cb) -> pure (Just (x, stToPrim . cb))

popPush :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> a -> m (Maybe a)
popPush h = stToPrim . basicHeapPopPush h

popPush' :: (PrimMonad m, IntrusiveFixedHeap f a, s ~ PrimState m) => f s a -> a -> m (Maybe (a, Reprioritize m a))
popPush' h x = stToPrim $ do
  mPopped <- basicHeapPopPush' h x
  case mPopped of
    Nothing -> pure Nothing
    Just (y, cb) -> pure (Just (y, stToPrim . cb))

pushPop :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> a -> m a
pushPop h = stToPrim . basicHeapPushPop h

pushPop' :: (PrimMonad m, IntrusiveFixedHeap f a, s ~ PrimState m) => f s a -> a -> m (Reprioritize m a, a)
pushPop' h x = stToPrim $ do
  (cb, y) <- basicHeapPushPop' h x
  pure (stToPrim . cb, y)

reprioritize :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> Int -> a -> m ()
reprioritize h i = stToPrim . basicHeapReprioritize h i

clear :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> m ()
clear = stToPrim . basicClear

push :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> a -> m ()
push h = stToPrim . basicHeapPush h

pop :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> m (Maybe a)
pop = stToPrim . basicHeapPop

push' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Reprioritize m a)
push' h x = stToPrim $ do
  cb <- basicHeapPush' h x
  pure $ stToPrim . cb

size :: (PrimMonad m, FixedHeap f a, s ~ PrimState m) => f s a -> m Int
size = stToPrim . basicHeapSize

swap :: (FixedHeapable f a) => f s a -> Int -> Int -> ST s ()
swap h i j = do
  xi <- basicHeapRead h i
  xj <- basicHeapRead h j
  uncurry (basicHeapWrite h i) xj
  uncurry (basicHeapWrite h j) xi

genericHeapify :: (FixedHeapable f a) => f s a -> Int -> ST s ()
genericHeapify h initIx =
  let go len i = do
        xi <- fst <$> basicHeapRead h i

        let f j =
              if j < len
                then Just . ((,j)) . fst <$> basicHeapRead h j
                else pure Nothing
        mxl <- f (left i)
        mxr <- f (right i)
        let (_, largestIx) = maximumBy (basicHeapCompare h `on` fst) ((xi, i) : catMaybes [mxl, mxr])

        when (largestIx /= i) $ do
          swap h i largestIx
          go len largestIx
   in do
        len <- basicHeapLength h
        go len initIx

genericPeek :: (FixedHeapable f a) => f s a -> ST s (Maybe a)
genericPeek h = fmap fst <$> basicHeapReadMaybe h 0

genericPeek' :: (FixedHeapable f a) => f s a -> ST s (Maybe (a, Handle f s))
genericPeek' h = basicHeapReadMaybe h 0

genericBuild :: (FixedHeapable f a) => f s a -> ST s ()
genericBuild h = do
  len <- basicHeapLength h
  let ixs = ((len - 1) `div` 2) `downTo` 0
  when (len > 0) $
    forM_ ixs (genericHeapify h)

genericHeapPopPush :: (FixedHeapable f a) => f s a -> a -> ST s (Maybe a)
genericHeapPopPush h x = fmap fst <$> genericHeapPopPush' h x

genericHeapPopPush' :: (FixedHeapable f a) => f s a -> a -> ST s (Maybe (a, Handle f s))
genericHeapPopPush' h x = do
  my <- genericPeek' h
  case my of
    Nothing ->
      -- The heap is empty; since we're fixed-size, we cannot write the new element.
      pure Nothing
    Just (y, yref) -> do
      -- Expire the element to be popped
      basicHeapExpireHandle h yref

      -- The heap has an element to pop. Re-use its location for the new element.
      xref <- basicHeapMakeHandle h
      basicHeapWrite h 0 x xref

      -- Recover the heap invariant
      genericHeapify h 0

      -- Return the popped element.
      pure (Just (y, xref))

genericHeapPushPop :: (FixedHeapable f a) => f s a -> a -> ST s a
genericHeapPushPop h x = snd <$> genericHeapPushPop' h x

genericHeapPushPop' :: (FixedHeapable f a) => f s a -> a -> ST s (Handle f s, a)
genericHeapPushPop' h x = do
  my <- genericPeek' h
  xref <- basicHeapMakeHandle h
  case my of
    Nothing ->
      -- The heap is empty; we'll leave it that way, and simply returned the to-be-pushed element
      pure (xref, x)
    Just (y, yref) -> do
      -- When the new element could be the maximum, return it without
      -- modifying the heap.
      if basicHeapCompare h x y /= LT
        then pure (xref, x)
        else do
          -- Expire the element to be popped
          basicHeapExpireHandle h yref

          -- Re-use the popped location for the new element.
          ref <- basicHeapMakeHandle h
          basicHeapWrite h 0 x ref

          -- Recover the heap invariant
          genericHeapify h 0

          -- Return the popped element.
          pure (xref, y)

genericHeapReprioritize :: (FixedHeapable f a) => f s a -> Int -> a -> ST s ()
genericHeapReprioritize h initIx x =
  let handleIncrease ix
        | ix <= 0 = pure () -- Increasing the root element is a no-op; we're done
        | otherwise = do
            -- We've increased a non-root element; maintain the heap invariant
            let pix = parent ix
            (p, _) <- basicHeapRead h pix
            when (basicHeapCompare h p x == LT) $ do
              swap h ix pix
              handleIncrease pix
   in do
        (incumbent, ref) <- basicHeapRead h initIx
        basicHeapWrite h initIx x ref
        if basicHeapCompare h x incumbent == LT
          then genericBuild h -- Recover the heap invariant by brute force
          else handleIncrease initIx -- Recover the heap invariant

genericHeapReprioritize' :: (FixedHeapable f a) => f s a -> Handle f s -> a -> ST s ()
genericHeapReprioritize' h ref x = do
  mIx <- basicHeapReadHandle h ref
  case mIx of
    Nothing -> pure ()
    Just (ix, _) -> genericHeapReprioritize h ix x

genericHeapPriority' :: (FixedHeapable f a) => f s a -> Handle f s -> ST s (Maybe a)
genericHeapPriority' h ref = fmap snd <$> basicHeapReadHandle h ref

genericHeapPush :: (Heapable f a) => f s a -> a -> ST s ()
genericHeapPush h x = void (genericHeapPush' h x)

genericHeapPush' :: (Heapable f a) => f s a -> a -> ST s (Handle f s)
genericHeapPush' h x = do
  -- Push the element to the end of the heap
  len <- basicHeapLength h
  ref <- basicHeapMakeHandle h

  basicHeapAppend h x ref

  -- Recover the heap property
  genericHeapReprioritize h len x
  pure ref

genericHeapPop :: (Heapable f a) => f s a -> ST s (Maybe a)
genericHeapPop h = do
  mx <- genericPeek' h
  case mx of
    Nothing -> pure Nothing
    Just (x, xref) -> do
      -- Move the last element to the first position
      len <- basicHeapLength h
      swap h 0 (len - 1)

      -- Shrink the heap to discard the final (now-popped) element
      basicHeapShrink h

      -- Expire the popped reference
      basicHeapExpireHandle h xref

      -- Recover the heap property
      when (len > 1) $
        genericHeapify h 0

      pure (Just x)

genericHeapSize :: (FixedHeapable f a) => f s a -> ST s Int
genericHeapSize = basicHeapLength

left :: Int -> Int
left i = 2 * i + 1

right :: Int -> Int
right = succ . left

parent :: Int -> Int
parent i = (i - 1) `div` 2
