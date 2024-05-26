{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Heap.Intrusive.Generic where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Kind
import Control.Monad

-- | An max heap with pointers to heap elements.
class (Ord a) => IntrusiveHeap f a where
  -- | The type of pointers to elements in the heap.
  type Handle f s :: Type

  basicHeapify :: f s a -> Int -> ST s ()

  basicHeapPush :: f s a -> a -> ST s ()
  basicHeapPush heap entry = void (basicHeapPush' heap entry)

  basicHeapPush' :: f s a -> a -> ST s (Handle f s)

  basicHeapPeek :: f s a -> ST s (Maybe a)
  basicHeapPeek = fmap (fmap fst) . basicHeapPeek'

  basicHeapPeek' :: f s a -> ST s (Maybe (a, Handle f s))

  basicHeapPop :: f s a -> ST s (Maybe a)

  basicHeapPopPush :: f s a -> a -> ST s (Maybe a)
  basicHeapPopPush heap entry = fmap fst (basicHeapPopPush' heap entry)

  basicHeapPopPush' :: f s a -> a -> ST s (Maybe a, Handle f s)

  basicHeapPushPop :: f s a -> a -> ST s a
  basicHeapPushPop heap entry = fmap snd (basicHeapPushPop' heap entry)

  basicHeapPushPop' :: f s a -> a -> ST s (Handle f s, a)

  basicHeapIncrease :: f s a -> Int -> a -> ST s ()
  basicHeapIncrease' :: f s a -> Handle f s -> a -> ST s ()

  basicHeapPriority' :: Handle f s -> ST s a

  basicHeapWriteEntry :: f s (a, Handle f s) -> Int -> a -> Handle f s -> ST s ()
  basicHeapWriteHandle :: f s (a, Handle f s) -> Int -> Handle f s -> ST s ()
  basicHeapSwapEntries :: f s (a, Handle f s) -> Int -> Int -> ST s ()

heapify :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> Int -> m ()
heapify h = stToPrim . basicHeapify h

heapPush :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m ()
heapPush h = stToPrim . basicHeapPush h

heapPush' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Handle f s)
heapPush' h = stToPrim . basicHeapPush' h

heapPeek :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> m (Maybe a)
heapPeek = stToPrim . basicHeapPeek

heapPop :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> m (Maybe a)
heapPop = stToPrim . basicHeapPop

heapPushPop :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m a
heapPushPop h = stToPrim . basicHeapPushPop h

heapPushPop' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Handle f s, a)
heapPushPop' h = stToPrim . basicHeapPushPop' h

heapPopPush :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Maybe a)
heapPopPush h = stToPrim . basicHeapPopPush h

heapPopPush' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Maybe a, Handle f s)
heapPopPush' h = stToPrim . basicHeapPopPush' h

heapIncrease :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> Int -> a -> m ()
heapIncrease h i = stToPrim . basicHeapIncrease h i

heapIncrease' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> Handle f s -> a -> m ()
heapIncrease' h ref = stToPrim . basicHeapIncrease' h ref
