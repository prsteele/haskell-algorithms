{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Heap.Heap where

import Algorithms.Utility
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Proxy
import qualified Data.Heap.Generic as HG
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Growable.Generic as GG
import qualified Data.Vector.Growable as GV
import qualified Data.Vector.Mutable as MV

newtype Heap v s a = Heap (v s a)

instance (GG.GrowVector v a, Ord a) => HG.Heap (Heap v) a where
  basicNew = heapBasicNew
  basicHeapify = heapBasicHeapify
  basicHeapPush = heapBasicPush
  basicHeapPeek = heapBasicPeek
  basicHeapPop = heapBasicPop
  basicHeapPopPush = heapBasicPopPush
  basicHeapPushPop = heapBasicPushPop
  basicHeapIncrease = heapBasicHeapIncrease

heapBasicNew :: (GG.GrowVector v a, Ord a) => Proxy (Heap v) -> Int -> ST s (Heap v s a)
heapBasicNew _ = fmap Heap . GG.new Proxy

heapBasicHeapify :: (GG.GrowVector v a, Ord a) => Heap v s a -> Int -> ST s ()
heapBasicHeapify (Heap gv) i =
  let heapBasicHeapify' len mv = do
        -- Find the larger of the parent and the left child
        largerIx <-
          if left i >= len
            then pure i
            else do
              xi <- MG.read mv i
              xl <- MG.read mv (left i)
              pure $
                if xl > xi
                  then left i
                  else i

        -- Find the larger of the incumbent and the right child
        largestIx <-
          if right i >= len
            then pure largerIx
            else do
              xLarger <- MG.read mv largerIx
              xr <- MG.read mv (right i)
              pure $
                if xr > xLarger
                  then right i
                  else largerIx
        when (largestIx /= i) $ do
          swap mv i largestIx
          heapBasicHeapify' i mv
   in do
        len <- GG.length gv
        GG.modifying gv (heapBasicHeapify' len)

heapBasicPush :: (GG.GrowVector v a, Ord a) => Heap v s a -> a -> ST s ()
heapBasicPush (Heap gv) x = do
  len <- GG.length gv
  GG.append gv x
  heapBasicHeapIncrease (Heap gv) len x

heapBasicPeek :: (GG.GrowVector v a) => Heap v s a -> ST s (Maybe a)
heapBasicPeek (Heap gv) = GG.modifying gv (`MG.readMaybe` 0)

heapBasicPop :: (GG.GrowVector v a, Ord a) => Heap v s a -> ST s (Maybe a)
heapBasicPop (Heap gv) =
  GG.modifying gv $ \mv -> do
    mx <- MG.readMaybe mv 0
    case mx of
      Nothing -> pure Nothing
      Just x -> do
        MG.read mv (MG.length mv - 1) >>= MG.write mv 0
        GG.shrink gv (MG.length mv - 1)
        heapBasicHeapify (Heap gv) 0
        pure (Just x)

heapBasicPopPush :: (GG.GrowVector v a, Ord a) => Heap v s a -> a -> ST s (Maybe a)
heapBasicPopPush (Heap gv) key =
  GG.modifying gv $ \mv -> do
    mx <- MG.readMaybe mv 0
    case mx of
      Nothing -> do
        -- The heap is currently empty. We'll return Nothing, but need
        -- to store the new element.
        GG.append gv key
        pure Nothing
      Just x -> do
        -- The heap has an element that we'll pop; re-use its location
        -- for the new element
        MG.write mv 0 key

        -- Recover the heap invariant
        heapBasicHeapify (Heap gv) 0

        -- Return the popped element
        pure (Just x)

heapBasicPushPop :: (GG.GrowVector v a, Ord a) => Heap v s a -> a -> ST s a
heapBasicPushPop (Heap gv) key =
  GG.modifying gv $ \mv -> do
    mx <- MG.readMaybe mv 0
    case mx of
      -- If there's nothing in the heap, we'll immediately pop the new
      -- element, so we can avoid touching the heap.
      Nothing -> pure key
      Just x ->
        if key > x
          then -- When the newly-pushed element is the max, we'll immediately
          -- pop the new element, so we can avoid touching the heap.
            pure key
          else do
            -- Re-use the popped element's location
            MG.write mv 0 key

            -- Recover the heap invariant
            heapBasicHeapify (Heap gv) 0

            -- Return the popped element
            pure x

heapBasicHeapIncrease :: (GG.GrowVector v a, Ord a) => Heap v s a -> Int -> a -> ST s ()
heapBasicHeapIncrease (Heap gv) initIx key =
  GG.modifying gv $ \mv -> do
    incumbent <- MG.read mv initIx
    when (key < incumbent) (error "cannot decrease key")
    MG.write mv initIx key

    let go ix
          | ix <= 0 = pure () -- We've increased the root element, we're done
          | otherwise = do
              -- We've increased a non-root element; maintain the heap invariant
              let pix = parent ix
              p <- MG.read mv pix
              when (p < key) $ do
                MG.swap mv ix pix
                go pix

    go initIx

left :: Int -> Int
left i = 2 * i + 1

right :: Int -> Int
right = succ . left

parent :: Int -> Int
parent i = (i - 1) `div` 2

new ::
  (PrimMonad m, GG.GrowVector v a, Ord a, s ~ PrimState m) =>
  Proxy (Heap v) ->
  Int ->
  m (Heap v s a)
new = HG.new

heapify ::
  (PrimMonad m, GG.GrowVector v a, Ord a, s ~ PrimState m) =>
  Heap v s a ->
  Int ->
  m ()
heapify = HG.heapify

heapPush ::
  (PrimMonad m, GG.GrowVector v a, s ~ PrimState m, Ord a) =>
  Heap v s a ->
  a ->
  m ()
heapPush = HG.heapPush

heapPeek ::
  (PrimMonad m, GG.GrowVector v a, s ~ PrimState m, Ord a) =>
  Heap v s a ->
  m (Maybe a)
heapPeek = HG.heapPeek

heapPop ::
  (PrimMonad m, GG.GrowVector v a, s ~ PrimState m, Ord a) =>
  Heap v s a ->
  m (Maybe a)
heapPop = HG.heapPop

heapPushPop ::
  (PrimMonad m, GG.GrowVector v a, s ~ PrimState m, Ord a) =>
  Heap v s a ->
  a ->
  m a
heapPushPop = HG.heapPushPop

heapPopPush ::
  (PrimMonad m, GG.GrowVector v a, s ~ PrimState m, Ord a) =>
  Heap v s a ->
  a ->
  m (Maybe a)
heapPopPush = HG.heapPopPush

heapIncrease ::
  (PrimMonad m, GG.GrowVector v a, s ~ PrimState m, Ord a) =>
  Heap v s a ->
  Int ->
  a ->
  m ()
heapIncrease = HG.heapIncrease

-- | A function converting a (proxy) growable vector to a (proxy) heap.
heapOf :: Proxy gv -> Proxy (Heap gv)
heapOf _ = Proxy

type IOHeapMVector = Heap (GV.GrowVector MV.MVector) RealWorld
type STHeapMVector s = Heap (GV.GrowVector MV.MVector) s
