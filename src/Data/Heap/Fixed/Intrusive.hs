{-# LANGUAGE TypeOperators #-}

module Data.Heap.Fixed.Intrusive
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
    peek,
    peek',
    popPush,
    popPush',
    pushPop,
    pushPop',
    size,
  )
where

import Control.Monad.Primitive
import Data.Bifunctor
import qualified Data.Heap.Generic as HG
import Data.STRef
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Mutable as MV

-- | A pointer to a heap element.
type Handle s = STRef s (Maybe Int)

-- | The handles to heap elements.
type Handles s = MV.MVector s (Handle s)

-- | A fixed-size binary max heap with pointers to heap elements.
--
-- An 'Heap v s a' uses storage 'v', state token 's', and contains values of type 'a'.
newtype Heap v s a = Heap {unHeap :: (a -> a -> Ordering, v s a, Handles s)}

instance (MG.MVector mv a) => HG.FixedHeapable (Heap mv) a where
  -- \| The type of handles to heap elements.
  type Handle (Heap mv) s = Handle s

  basicHeapCompare (Heap (cmp, _, _)) = cmp

  basicHeapRead (Heap (_, mv, refs)) i = (,) <$> MG.read mv i <*> MV.read refs i

  basicHeapReadMaybe (Heap (_, mv, refs)) i = do
    x <- MG.readMaybe mv i
    r <- MV.readMaybe refs i
    pure ((,) <$> x <*> r)

  basicHeapWrite (Heap (_, mv, refs)) i x r = do
    MG.write mv i x
    MV.write refs i r
    writeSTRef r (Just i)

  basicHeapLength (Heap (_, _, refs)) = pure (MV.length refs)

  basicHeapMakeHandle _ = newSTRef Nothing

  basicHeapExpireHandle _ = (`writeSTRef` Nothing)

  basicHeapReadHandle (Heap (_, mv, _)) ref = do
    mIx <- readSTRef ref
    case mIx of
      Nothing -> pure Nothing
      Just ix -> Just . (ix,) <$> MG.read mv ix

instance (MG.MVector mv a) => HG.FixedHeap (Heap mv) a where
  basicHeapBuild = HG.genericBuild
  basicHeapify = HG.genericHeapify
  basicHeapPeek = HG.genericPeek
  basicHeapPopPush = HG.genericHeapPopPush
  basicHeapPushPop = HG.genericHeapPushPop
  basicHeapReprioritize = HG.genericHeapReprioritize
  basicHeapSize = HG.genericHeapSize

instance (MG.MVector mv a) => HG.IntrusiveFixedHeap (Heap mv) a where
  basicHeapPeek' h = fmap (fmap (HG.genericHandleToCallback h)) <$> HG.genericPeek' h
  basicHeapPopPush' h x = fmap (fmap (HG.genericHandleToCallback h)) <$> HG.genericHeapPopPush' h x
  basicHeapPushPop' h x = first (HG.genericHandleToCallback h) <$> HG.genericHeapPushPop' h x

heap :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => (a -> a -> Ordering) -> mv s a -> m (Heap mv s a)
heap cmp mv = stToPrim $ do
  refs <- MG.generateM (MG.length mv) (newSTRef . Just)
  let h = Heap (cmp, mv, refs)
  build h
  pure h

fromHeap :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m (mv s a)
fromHeap (Heap (_, mv, _)) = pure mv

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

peek' :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m (Maybe (a, HG.Reprioritize m a))
peek' = HG.peek'

popPush' :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> a -> m (Maybe (a, HG.Reprioritize m a))
popPush' = HG.popPush'

pushPop' :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> a -> m (HG.Reprioritize m a, a)
pushPop' = HG.pushPop'

size :: (PrimMonad m, MG.MVector mv a, s ~ PrimState m) => Heap mv s a -> m Int
size = HG.size
