{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Heap.Generic where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Kind
import Data.Proxy

-- | A max heap.
class (Ord a) => Heap f a where
  basicNew :: Proxy f -> Int -> ST s (f s a)
  basicHeapify :: f s a -> Int -> ST s ()
  basicHeapPush :: f s a -> a -> ST s ()
  basicHeapPeek :: f s a -> ST s (Maybe a)
  basicHeapPop :: f s a -> ST s (Maybe a)
  basicHeapPopPush :: f s a -> a -> ST s (Maybe a)
  basicHeapPushPop :: f s a -> a -> ST s a
  basicHeapIncrease :: f s a -> Int -> a -> ST s ()

-- | An max heap with pointers to heap elements.
class (Heap f a) => IntrusiveHeap f a where
  -- | The type of pointers to elements in the heap.
  type Handle f s :: Type

  basicHeapPush' :: f s a -> a -> ST s (Handle f s)
  basicHeapPopPush' :: f s a -> a -> ST s (Maybe a, Handle f s)
  basicHeapPushPop' :: f s a -> a -> ST s (Handle f s, a)
  basicHeapIncrease' :: f s a -> Handle f s -> a -> ST s ()
  basicHeapPriority' :: f s a -> Handle f s -> ST s (Maybe a)

new :: (PrimMonad m, Heap f a, s ~ PrimState m) => Proxy f -> Int -> m (f s a)
new f = stToPrim . basicNew f

heapify :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> Int -> m ()
heapify h = stToPrim . basicHeapify h

heapPush :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> a -> m ()
heapPush h = stToPrim . basicHeapPush h

heapPush' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Handle f s)
heapPush' h = stToPrim . basicHeapPush' h

heapPeek :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> m (Maybe a)
heapPeek = stToPrim . basicHeapPeek

heapPop :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> m (Maybe a)
heapPop = stToPrim . basicHeapPop

heapPushPop :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> a -> m a
heapPushPop h = stToPrim . basicHeapPushPop h

heapPushPop' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Handle f s, a)
heapPushPop' h = stToPrim . basicHeapPushPop' h

heapPopPush :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> a -> m (Maybe a)
heapPopPush h = stToPrim . basicHeapPopPush h

heapPopPush' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> a -> m (Maybe a, Handle f s)
heapPopPush' h = stToPrim . basicHeapPopPush' h

heapIncrease :: (PrimMonad m, Heap f a, s ~ PrimState m) => f s a -> Int -> a -> m ()
heapIncrease h i = stToPrim . basicHeapIncrease h i

heapIncrease' :: (PrimMonad m, IntrusiveHeap f a, s ~ PrimState m) => f s a -> Handle f s -> a -> m ()
heapIncrease' h ref = stToPrim . basicHeapIncrease' h ref
