{-# LANGUAGE TypeOperators #-}

module Data.Heap.Fixed
  ( -- * Fixed-sized binary max heaps
    Heap (..),

    -- ** Construction and destruction
    heap,
    fromHeap,

    -- ** Fixed-size heap operations
    build,
    heapify,
    peek,
    popPush,
    pushPop,
    reprioritize,
    size,
  )
where

import Control.Monad.Primitive
import qualified Data.Heap.Generic as HG
import qualified Data.Vector.Generic.Mutable as MG

-- | A fixed-size binary max heap.
--
-- A 'FixedHeap v s a' uses storage 'v', state token 's', and contains values of type 'a'.
newtype Heap mv s a = Heap {unHeap :: (a -> a -> Ordering, mv s a)}

instance (MG.MVector mv a) => HG.FixedHeapable (Heap mv) a where
  type Handle (Heap mv) s = ()

  basicHeapCompare (Heap (cmp, _)) = cmp
  basicHeapRead (Heap (_, mv)) i = (,()) <$> MG.read mv i
  basicHeapReadMaybe (Heap (_, mv)) i = fmap (,()) <$> MG.readMaybe mv i
  basicHeapWrite (Heap (_, mv)) i x _ = MG.write mv i x
  basicHeapLength (Heap (_, mv)) = pure (MG.length mv)
  basicHeapMakeHandle _ = pure ()
  basicHeapExpireHandle _ _ = pure ()
  basicHeapReadHandle _ _ = pure Nothing

instance (MG.MVector mv a) => HG.FixedHeap (Heap mv) a where
  basicHeapBuild = HG.genericBuild
  basicHeapify = HG.genericHeapify
  basicHeapPeek = HG.genericPeek
  basicHeapPopPush = HG.genericHeapPopPush
  basicHeapPushPop = HG.genericHeapPushPop
  basicHeapReprioritize = HG.genericHeapReprioritize
  basicHeapSize = HG.genericHeapSize

heap :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => (a -> a -> Ordering) -> mv s a -> m (Heap mv s a)
heap cmp mv = do
  let h = Heap (cmp, mv)
  build h
  pure h

fromHeap :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m (mv s a)
fromHeap (Heap (_, mv)) = pure mv

build :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m ()
build = HG.build

heapify :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> Int -> m ()
heapify = HG.heapify

peek :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m (Maybe a)
peek = HG.peek

popPush :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> a -> m (Maybe a)
popPush = HG.popPush

pushPop :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> a -> m a
pushPop = HG.pushPop

reprioritize :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> Int -> a -> m ()
reprioritize = HG.reprioritize

size :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m Int
size = HG.size
