{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Heap.Intrusive where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Heap.Generic as HG
import Data.Heap.Heap (left, parent, right)
import Data.Proxy
import Data.STRef
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Growable.Generic as GG

type IntrusiveHandle s = STRef s (Maybe Int)

newtype IntrusiveHeap v s a = IntrusiveHeap (v s (a, IntrusiveHandle s))

instance
  ( forall s. GG.GrowVector v (a, IntrusiveHandle s),
    Ord a
  ) =>
  HG.Heap (IntrusiveHeap v) a
  where
  basicHeapify = intrusiveHeapBasicHeapify
  basicHeapPush = intrusiveHeapBasicPush
  basicHeapPeek = intrusiveHeapBasicPeek
  basicHeapPop = intrusiveHeapBasicPop
  basicHeapPopPush = intrusiveHeapBasicPopPush
  basicHeapPushPop = intrusiveHeapBasicPushPop
  basicHeapIncrease = intrusiveHeapBasicHeapIncrease

instance
  ( forall s. GG.GrowVector v (a, IntrusiveHandle s),
    Ord a
  ) =>
  HG.IntrusiveHeap (IntrusiveHeap v) a
  where
  type Handle (IntrusiveHeap v) s = IntrusiveHandle s
  basicHeapPush' = intrusiveHeapBasicPush'
  basicHeapPopPush' = intrusiveHeapBasicPopPush'
  basicHeapPushPop' = intrusiveHeapBasicPushPop'
  basicHeapIncrease' = intrusiveHeapBasicHeapIncrease'
  basicHeapPriority' = intrusiveHeapBasicHeapPriority'

intrusiveSwap ::
  (MG.MVector v (a, IntrusiveHandle s)) =>
  v s (a, IntrusiveHandle s) ->
  Int ->
  Int ->
  ST s ()
intrusiveSwap v i j = do
  MG.swap v i j

  -- After the swap, inform each handle about their new position
  MG.read v i >>= \(_, ref) -> writeSTRef ref (Just i)
  MG.read v j >>= \(_, ref) -> writeSTRef ref (Just j)

-- | Mark a handle as no longer pointing to a heap.
expireRef :: STRef s (Maybe Int) -> ST s ()
expireRef = flip writeSTRef Nothing

-- | Write an element to an index, updating the handle.
--
-- This is an internal helper function.
intrusiveWrite ::
  (MG.MVector v (a, IntrusiveHandle s)) =>
  v s (a, IntrusiveHandle s) ->
  Int ->
  a ->
  IntrusiveHandle s ->
  ST s ()
intrusiveWrite v ix x xRef = do
  MG.write v ix (x, xRef)
  writeSTRef xRef (Just ix)

intrusiveHeapBasicNew :: (GG.GrowVector v (a, IntrusiveHandle s), Ord a) => Proxy (IntrusiveHeap v) -> Int -> ST s (IntrusiveHeap v s a)
intrusiveHeapBasicNew _ = fmap IntrusiveHeap . GG.new Proxy

intrusiveHeapBasicHeapify ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  Int ->
  ST s ()
intrusiveHeapBasicHeapify (IntrusiveHeap gv) i =
  let heapBasicHeapify' len mv = do
        -- Find the larger of the parent and the left child
        largerIx <-
          if left i >= len
            then pure i
            else do
              (xi, _) <- MG.read mv i
              (xl, _) <- MG.read mv (left i)
              pure $
                if xl > xi
                  then left i
                  else i

        -- Find the larger of the incumbent and the right child
        largestIx <-
          if right i >= len
            then pure largerIx
            else do
              (xLarger, _) <- MG.read mv largerIx
              (xr, _) <- MG.read mv (right i)
              pure $
                if xr > xLarger
                  then right i
                  else largerIx
        when (largestIx /= i) $ do
          intrusiveSwap mv i largestIx
          heapBasicHeapify' i mv
   in do
        len <- GG.length gv
        GG.modifying gv (heapBasicHeapify' len)

intrusiveHeapBasicPush ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  ST s ()
intrusiveHeapBasicPush h x = void (intrusiveHeapBasicPush' h x)

intrusiveHeapBasicPeek ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  ST s (Maybe a)
intrusiveHeapBasicPeek (IntrusiveHeap gv) =
  fmap (fmap fst) (GG.modifying gv (`MG.readMaybe` 0))

intrusiveHeapBasicPop ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  ST s (Maybe a)
intrusiveHeapBasicPop (IntrusiveHeap gv) =
  GG.modifying gv $ \mv -> do
    mx <- MG.readMaybe mv 0
    case mx of
      Nothing -> pure Nothing
      Just (x, _) -> do
        -- Move the last element to the first position
        MG.read mv (MG.length mv - 1) >>= uncurry (intrusiveWrite mv 0)

        -- Shrink the vector to account for the popped element
        GG.shrink gv (MG.length mv - 1)

        -- Recover the heap property
        intrusiveHeapBasicHeapify (IntrusiveHeap gv) 0

        -- Return the popped element
        pure (Just x)

intrusiveHeapBasicPopPush ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  ST s (Maybe a)
intrusiveHeapBasicPopPush heap new =
  fmap fst (intrusiveHeapBasicPopPush' heap new)

intrusiveHeapBasicPushPop ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  ST s a
intrusiveHeapBasicPushPop heap new =
  fmap snd (intrusiveHeapBasicPushPop' heap new)

intrusiveHeapBasicHeapIncrease ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  Int ->
  a ->
  ST s ()
intrusiveHeapBasicHeapIncrease (IntrusiveHeap gv) ix new =
  GG.modifying gv $ \mv -> do
    (_, ref) <- MG.read mv ix
    intrusiveHeapBasicHeapIncrease' (IntrusiveHeap gv) ref new

intrusiveHeapBasicPush' ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  ST s (IntrusiveHandle s)
intrusiveHeapBasicPush' (IntrusiveHeap gv) x = do
  len <- GG.length gv
  ref <- newSTRef (Just len)
  GG.append gv (x, ref)
  intrusiveHeapBasicHeapIncrease (IntrusiveHeap gv) len x
  pure ref

intrusiveHeapBasicPopPush' ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  ST s (Maybe a, IntrusiveHandle s)
intrusiveHeapBasicPopPush' (IntrusiveHeap gv) new =
  GG.modifying gv $ \mv -> do
    mx <- MG.readMaybe mv 0

    -- Create a handle for the new element. In all cases, the new
    -- element is initially inserted into index 0.
    newRef <- newSTRef (Just 0)

    case mx of
      Nothing -> do
        -- The heap is currently empty. We'll return Nothing, but need
        -- to store the new element.
        GG.append gv (new, newRef)
        pure (Nothing, newRef)
      Just (popped, poppedRef) -> do
        -- Expire the popped reference
        expireRef poppedRef

        -- Re-use the popped element's position
        intrusiveWrite mv 0 new newRef

        -- Recover the heap invariant
        intrusiveHeapBasicHeapify (IntrusiveHeap gv) 0

        -- Return the popped element
        pure (Just popped, newRef)

intrusiveHeapBasicPushPop' ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  ST s (IntrusiveHandle s, a)
intrusiveHeapBasicPushPop' (IntrusiveHeap gv) new =
  GG.modifying gv $ \mv -> do
    mx <- MG.readMaybe mv 0

    -- Create a handle for the new element.
    newRef <- newSTRef Nothing

    case mx of
      -- If there's nothing in the heap, we'll immediately pop the new
      -- element, so we can avoid touching the heap.
      Nothing -> pure (newRef, new)
      Just (popped, poppedRef) ->
        if new > popped
          then -- When the newly-pushed element is the max, we'll immediately
          -- pop the new element, so we can avoid touching the heap.
            pure (newRef, new)
          else do
            -- Expire the popped reference
            expireRef poppedRef

            -- Re-use the popped element's location
            intrusiveWrite mv 0 new newRef

            -- Recover the heap invariant
            intrusiveHeapBasicHeapify (IntrusiveHeap gv) 0

            -- Return the popped element
            pure (newRef, popped)

intrusiveHeapBasicHeapIncrease' ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  IntrusiveHandle s ->
  a ->
  ST s ()
intrusiveHeapBasicHeapIncrease' (IntrusiveHeap gv) ref new =
  GG.modifying gv $ \mv -> do
    mInitIx <- readSTRef ref

    case mInitIx of
      Nothing -> pure () -- We can't increase the priority of an element not in the heap
      Just initIx -> do
        (incumbent, _) <- MG.read mv initIx
        when (new < incumbent) (error "cannot decrease key")

        -- Write the updated element in the same position
        MG.write mv initIx (new, ref)

        let go ix
              | ix <= 0 = pure () -- We've increased the root element, we're done
              | otherwise = do
                  -- We've increased a non-root element; maintain the heap invariant
                  let pix = parent ix
                  (p, _) <- MG.read mv ix
                  when (p < new) $ do
                    intrusiveSwap mv pix ix
                    go pix

        go initIx

intrusiveHeapBasicHeapPriority' ::
  (GG.GrowVector v (a, IntrusiveHandle s), Ord a) =>
  IntrusiveHeap v s a ->
  IntrusiveHandle s ->
  ST s (Maybe a)
intrusiveHeapBasicHeapPriority' (IntrusiveHeap gv) ref =
  GG.modifying gv $ \mv -> do
    mIx <- readSTRef ref
    case mIx of
      Nothing -> pure Nothing
      Just ix -> fmap (pure . fst) (MG.read mv ix)


heapify ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  Int ->
  m ()
heapify = HG.heapify

heapPush ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  m ()
heapPush = HG.heapPush

heapPeek ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  m (Maybe a)
heapPeek = HG.heapPeek

heapPop ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  m (Maybe a)
heapPop = HG.heapPop

heapPushPop ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  m a
heapPushPop = HG.heapPushPop

heapPopPush ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  m (Maybe a)
heapPopPush = HG.heapPopPush

heapIncrease ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  Int ->
  a ->
  m ()
heapIncrease = HG.heapIncrease

heapPush' ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  m (IntrusiveHandle s)
heapPush' = HG.heapPush'

heapPopPush' ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  m (Maybe a, IntrusiveHandle s)
heapPopPush' = HG.heapPopPush'

heapPushPop' ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  a ->
  m (IntrusiveHandle s, a)
heapPushPop' = HG.heapPushPop'

heapIncrease' ::
  (forall t. GG.GrowVector v (a, IntrusiveHandle t), PrimMonad m, s ~ PrimState m, Ord a) =>
  IntrusiveHeap v s a ->
  IntrusiveHandle s ->
  a ->
  m ()
heapIncrease' = HG.heapIncrease'
