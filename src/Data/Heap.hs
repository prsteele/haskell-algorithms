{-# LANGUAGE TypeOperators #-}

module Data.Heap
  ( -- * Dynamically-sized binary max heaps
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

    -- ** Size-changing heap operations
    clear,
    push,
    pop,
  )
where

import Control.Monad.Primitive
import qualified Data.Heap.Generic as HG
import qualified Data.Vector.Growable.Generic as GG

-- | A dynamically-sized binary max heap.
--
-- A 'Heap gv s a' uses storage 'gv', state token 's', and contains values of type 'a'.
newtype Heap gv s a = Heap {unHeap :: (a -> a -> Ordering, gv s a)}

instance (GG.GrowVector gv a) => HG.FixedHeapable (Heap gv) a where
  type Handle (Heap gv) s = ()

  basicHeapCompare (Heap (cmp, _)) = cmp
  basicHeapRead (Heap (_, gv)) i = (,()) <$> GG.read gv i
  basicHeapReadMaybe (Heap (_, gv)) i = fmap (,()) <$> GG.readMaybe gv i
  basicHeapWrite (Heap (_, gv)) i x _ = GG.write gv i x
  basicHeapLength (Heap (_, gv)) = GG.length gv
  basicHeapMakeHandle _ = pure ()
  basicHeapExpireHandle _ _ = pure ()
  basicHeapReadHandle _ _ = pure Nothing

instance (GG.GrowVector gv a) => HG.Heapable (Heap gv) a where
  basicHeapAppend (Heap (_, gv)) x _ = GG.append gv x
  basicHeapShrink (Heap (_, gv)) = do
    len <- GG.length gv
    GG.shrink gv (len - 1)

instance (Ord a, GG.GrowVector gv a) => HG.FixedHeap (Heap gv) a where
  basicHeapBuild = HG.genericBuild
  basicHeapify = HG.genericHeapify
  basicHeapPeek = HG.genericPeek
  basicHeapPopPush = HG.genericHeapPopPush
  basicHeapPushPop = HG.genericHeapPushPop
  basicHeapReprioritize = HG.genericHeapReprioritize
  basicHeapSize = HG.genericHeapSize

instance (Ord a, GG.GrowVector gv a) => HG.Heap (Heap gv) a where
  basicClear (Heap (_, gv)) = GG.shrink gv 0
  basicHeapPop = HG.genericHeapPop
  basicHeapPush = HG.genericHeapPush

clear :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m ()
clear = HG.clear

heap :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => (a -> a -> Ordering) -> gv s a -> m (Heap gv s a)
heap cmp gv =
  let h = Heap (cmp, gv)
   in stToPrim (HG.genericBuild h) >> pure h

fromHeap :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m (gv s a)
fromHeap (Heap (_, gv)) = pure gv

build :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m ()
build = HG.build

heapify :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> Int -> m ()
heapify = HG.heapify

peek :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m (Maybe a)
peek = HG.peek

popPush :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> a -> m (Maybe a)
popPush = HG.popPush

pushPop :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> a -> m a
pushPop = HG.pushPop

reprioritize :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> Int -> a -> m ()
reprioritize = HG.reprioritize

size :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m Int
size = HG.size

push :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> a -> m ()
push = HG.push

pop :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m (Maybe a)
pop = HG.pop
