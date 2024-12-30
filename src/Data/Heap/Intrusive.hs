{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Data.Heap.Intrusive
  ( -- * Dynamically-sized binary max heaps
    Heap (..),
    HG.Reprioritize,

    -- ** Construction and destruction
    heap,
    fromHeap,

    -- ** Fixed-size heap operations
    build,
    heapify,
    reprioritize,
    peek',
    peek,
    popPush',
    popPush,
    pushPop',
    pushPop,
    size,

    -- ** Size-changing heap operations
    clear,
    pop,
    push',
    push,
  )
where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bifunctor
import qualified Data.Heap.Generic as HG
import Data.STRef
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Growable as GV
import qualified Data.Vector.Growable.Generic as GG

-- | A pointer to a heap element.
type Handle s = STRef s (Maybe Int)

-- | The handles to heap elements.
type Handles s = GV.GrowMVector s (Handle s)

-- | A dynamically-sized binary max heap with pointers to heap elements.
--
-- A 'Heap v s a' uses storage 'v', state token 's', and contains values of type 'a'.
newtype Heap gv s a = Heap {unHeap :: (a -> a -> Ordering, gv s a, Handles s)}

instance (GG.GrowVector gv a) => HG.FixedHeapable (Heap gv) a where
  type Handle (Heap gv) s = Handle s

  basicHeapCompare (Heap (cmp, _, _)) = cmp

  basicHeapRead :: (Heap gv) s a -> Int -> ST s (a, Handle s)
  basicHeapRead (Heap (_, gv, refs)) i = (,) <$> GG.read gv i <*> GV.read refs i

  basicHeapReadMaybe (Heap (_, gv, refs)) i = do
    x <- GG.readMaybe gv i
    r <- GV.readMaybe refs i
    pure ((,) <$> x <*> r)

  basicHeapWrite (Heap (_, gv, refs)) i x r = do
    GG.write gv i x
    GV.write refs i r
    writeSTRef r (Just i)

  basicHeapLength (Heap (_, _, refs)) = GV.length refs

  basicHeapMakeHandle _ = newSTRef Nothing

  basicHeapExpireHandle _ h = writeSTRef h Nothing

  basicHeapReadHandle (Heap (_, gv, _)) ref = do
    mIx <- readSTRef ref
    case mIx of
      Nothing -> pure Nothing
      Just ix -> Just . (ix,) <$> GG.read gv ix

instance (GG.GrowVector gv a) => HG.Heapable (Heap gv) a where
  basicHeapAppend (Heap (_, gv, refs)) x ref = do
    len <- GV.length refs
    writeSTRef ref (Just len)
    GG.append gv x
    GV.append refs ref

  basicHeapShrink (Heap (_, gv, refs)) = do
    len <- GV.length refs
    GG.shrink gv (len - 1)
    GV.shrink refs (len - 1)

instance (Ord a, GG.GrowVector gv a) => HG.FixedHeap (Heap gv) a where
  basicHeapBuild = HG.genericBuild
  basicHeapify = HG.genericHeapify
  basicHeapPeek = HG.genericPeek
  basicHeapPopPush = HG.genericHeapPopPush
  basicHeapPushPop = HG.genericHeapPushPop
  basicHeapReprioritize = HG.genericHeapReprioritize
  basicHeapSize = HG.genericHeapSize

instance (Ord a, GG.GrowVector gv a) => HG.IntrusiveFixedHeap (Heap gv) a where
  basicHeapPeek' h = fmap (fmap (HG.genericHandleToCallback h)) <$> HG.genericPeek' h
  basicHeapPopPush' h x = fmap (fmap (HG.genericHandleToCallback h)) <$> HG.genericHeapPopPush' h x
  basicHeapPushPop' h x = first (HG.genericHandleToCallback h) <$> HG.genericHeapPushPop' h x

instance (Ord a, GG.GrowVector gv a) => HG.Heap (Heap gv) a where
  basicClear (Heap (_, gv, refs)) = GG.shrink gv 0 >> GV.shrink refs 0

  basicHeapPop = HG.genericHeapPop
  basicHeapPush = HG.genericHeapPush

instance (Ord a, GG.GrowVector gv a) => HG.IntrusiveHeap (Heap gv) a where
  basicHeapPush' h x = HG.genericHandleToCallback h <$> HG.genericHeapPush' h x

heap :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => (a -> a -> Ordering) -> gv s a -> m (Heap gv s a)
heap cmp gv = stToPrim $ do
  refs <- GG.withMVector gv $ \mv -> MG.generateM (MG.length mv) (newSTRef . Just)
  grefs <- GV.fromMVector refs
  let h = Heap (cmp, gv, grefs)
  build h
  pure h

fromHeap :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m (gv s a)
fromHeap (Heap (_, gv, _)) = pure gv

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

peek' :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m (Maybe (a, HG.Reprioritize m a))
peek' = HG.peek'

popPush' :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> a -> m (Maybe (a, HG.Reprioritize m a))
popPush' = HG.popPush'

pushPop' :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> a -> m (HG.Reprioritize m a, a)
pushPop' = HG.pushPop'

clear :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> m ()
clear = HG.clear

push' :: (Ord a, PrimMonad m, GG.GrowVector gv a, s ~ PrimState m) => Heap gv s a -> a -> m (HG.Reprioritize m a)
push' = HG.push'
