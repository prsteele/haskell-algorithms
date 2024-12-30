module Data.Heap.HeapSpec where

import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Algorithms.Utility
import Control.Monad
import Control.Monad.ST
import Data.Functor.Foldable
import qualified Data.Heap as H
import qualified Data.Heap.Fixed as FH
import qualified Data.Heap.Fixed.Intrusive as FHI
import Data.Heap.Generic (left, parent, right)
import qualified Data.Heap.Generic as HG
import qualified Data.Heap.Intrusive as HI
import qualified Data.Vector as V
import qualified Data.Vector.Growable as GV
import qualified Data.Vector.Mutable as MV
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Indexing operations" $ do
    prop "parent . left = id" $ \(NonNegative i) -> parent (left i) == i
    prop "parent . right = id" $ \(NonNegative i) -> parent (right i) == i
    prop "parent i < left i < right i" $ \(NonNegative i) -> parent i < left i && left i < right i
  describe "Construction" $ do
    prop "FixedHeap" $ \xs ->
      let v = V.fromList @Int xs
       in runST $ V.thaw v >>= FH.heap compare >>= fixedHeapIsHeapified
    prop "Heap" $ \xs ->
      let v = V.fromList @Int xs
       in runST $ GV.fromVector v >>= H.heap compare >>= heapIsHeapified
    prop "IntrusiveFixedHeap" $ \xs ->
      let v = V.fromList @Int xs
       in runST $ V.thaw v >>= FHI.heap compare >>= intrusiveFixedHeapIsHeapified
    prop "IntrusiveHeap" $ \xs ->
      let v = V.fromList @Int xs
       in runST $ GV.fromVector v >>= HI.heap compare >>= intrusiveHeapIsHeapified
  describe "HeapSort can be implemented with" $ do
    prop "a fixed heap" $ \(IntVector v) ->
      isSortingAlgorithm heapSortFixedHeap v
    prop "an intrusive fixed heap" $ \(IntVector v) ->
      isSortingAlgorithm heapSortIntrusiveFixedHeap v
    prop "a heap" $ \(IntVector v) ->
      isSortingAlgorithm heapSortHeap v
    prop "an intrusive heap" $ \(IntVector v) ->
      isSortingAlgorithm heapSortIntrusiveHeap v
  describe "Intrusive heaps" $ do
    prop "expire references" $ \nonEmptyElems ->
      let elems = getNonEmpty @Integer nonEmptyElems

          -- Increase some values, decrease others
          mogrify i
            | even i = i `div` 2
            | otherwise = 2 * i
       in runST $ do
            h <- HI.heap compare =<< GV.empty 0
            reprios <- forM elems (HI.push' h)

            -- Reprioritize existing elements
            forM_ (zip elems reprios) $ \(x, re) -> re (mogrify x)

            -- Pop all elements
            forM_ elems $ \_ -> HI.pop h

            -- Reprioritize all (now-expired) elements. This should be
            -- a no-op, but it certainly shouldn't explode.
            forM_ (zip elems reprios) $ \(_, re) -> re 0

            pure True
  describe "arbitrary fixed heap operations maintain the heap property" $ do
    prop "for FixedHeaps" $ \elems ops -> runST $ do
      h <- FH.heap compare =<< V.thaw (V.fromList (getNonEmpty @Int elems))
      checkFixedHeapOps h fixedHeapIsHeapified ops
    prop "for FixedIntrusiveHeaps" $ \elems ops -> runST $ do
      h <- FHI.heap compare =<< V.thaw (V.fromList (getNonEmpty @Int elems))
      checkFixedHeapOps h intrusiveFixedHeapIsHeapified ops
    prop "for Heaps" $ \elems ops -> runST $ do
      h <- H.heap compare =<< GV.fromVector (V.fromList (getNonEmpty @Int elems))
      checkFixedHeapOps h heapIsHeapified ops
    prop "for IntrusiveHeaps" $ \elems ops -> runST $ do
      h <- HI.heap compare =<< GV.fromVector (V.fromList (getNonEmpty @Int elems))
      checkFixedHeapOps h intrusiveHeapIsHeapified ops

heapSortFixedHeap :: V.Vector Int -> V.Vector Int
heapSortFixedHeap v =
  runST $ do
    mv <- V.thaw v
    _ <- FH.heap compare mv

    forM_ ((MV.length mv - 1) `downTo` 1) $ \i -> do
      MV.swap mv 0 i
      FH.heapify (FH.Heap (compare, (MV.slice 0 i mv))) 0

    V.freeze mv

heapSortIntrusiveFixedHeap :: V.Vector Int -> V.Vector Int
heapSortIntrusiveFixedHeap v =
  runST $ do
    mv <- V.thaw v
    _ <- FHI.heap compare mv

    forM_ ((MV.length mv - 1) `downTo` 1) $ \i -> do
      MV.swap mv 0 i

      -- We can't (easily, cheaply) allocate a slice to an intrusive
      -- heap, so we just pay to heapify it again.
      h' <- FHI.heap compare (MV.slice 0 i mv)
      FHI.heapify h' 0

    V.freeze mv

heapSortHeap :: V.Vector Int -> V.Vector Int
heapSortHeap v =
  runST $ do
    gv <- GV.fromVector v
    h <- H.heap compare gv

    len <- H.size h
    forM_ ((len - 1) `downTo` 1) $ \i -> do
      GV.swap gv 0 i

      -- Growables don't support slices, since growing a slice would
      -- imply strange behavior. So, just make a new one.
      gv' <- GV.fromMVector =<< GV.withMVector gv (pure . MV.slice 0 i)

      H.heapify (H.Heap (compare, gv')) 0

    GV.freeze gv

heapSortIntrusiveHeap :: V.Vector Int -> V.Vector Int
heapSortIntrusiveHeap v =
  runST $ do
    gv <- GV.fromVector v
    h <- HI.heap compare gv

    len <- HI.size h
    forM_ ((len - 1) `downTo` 1) $ \i -> do
      GV.swap gv 0 i

      -- Growables don't support slices, since growing a slice would
      -- imply strange behavior. So, just make a new one.
      gv' <- GV.fromMVector =<< GV.withMVector gv (pure . MV.slice 0 i)

      -- We can't (easily, cheaply) allocate a slice to an intrusive
      -- heap, so we just pay to heapify it again.
      h' <- HI.heap compare gv'

      HI.heapify h' 0

    GV.freeze gv

data FixedHeapOp a
  = FHPeek
  | FHHeapify Int
  | FHBuild
  | FHPopPush a
  | FHPushPop a
  deriving (Show)

instance (Arbitrary a) => Arbitrary (FixedHeapOp a) where
  arbitrary = do
    i <- fmap (`mod` (5 :: Int)) arbitrary
    case i of
      0 -> pure FHPeek
      1 -> FHHeapify <$> arbitrary
      2 -> pure FHBuild
      3 -> FHPopPush <$> arbitrary
      _ -> FHPushPop <$> arbitrary

applyFixedHeapOp :: (Ord a, HG.FixedHeap f a) => f s a -> FixedHeapOp a -> ST s ()
applyFixedHeapOp h FHPeek = void $ HG.peek h
applyFixedHeapOp h (FHHeapify i) = do
  n <- HG.size h
  HG.heapify h (i `mod` n)
applyFixedHeapOp h FHBuild = HG.build h
applyFixedHeapOp h (FHPopPush x) = void $ HG.popPush h x
applyFixedHeapOp h (FHPushPop x) = void $ HG.pushPop h x

fixedHeapIsHeapified :: (Ord a) => FH.Heap MV.MVector s a -> ST s Bool
fixedHeapIsHeapified (FH.Heap (_, mv)) = isHeapified <$> V.freeze mv

intrusiveFixedHeapIsHeapified :: (Ord a) => FHI.Heap MV.MVector s a -> ST s Bool
intrusiveFixedHeapIsHeapified (FHI.Heap (_, mv, _)) = isHeapified <$> V.freeze mv

heapIsHeapified :: (Ord a) => H.Heap GV.GrowMVector s a -> ST s Bool
heapIsHeapified (H.Heap (_, gv)) = isHeapified <$> GV.freeze gv

intrusiveHeapIsHeapified :: (Ord a) => HI.Heap GV.GrowMVector s a -> ST s Bool
intrusiveHeapIsHeapified (HI.Heap (_, gv, _)) = isHeapified <$> GV.freeze gv

isHeapified :: (Ord a) => V.Vector a -> Bool
isHeapified v =
  let size = V.length v
      f i x = (left i `isNoMoreThan` x) && (right i `isNoMoreThan` x)
      isNoMoreThan i x
        | i < size = v V.! i <= x
        | otherwise = True
   in V.and (V.imap f v)

checkFixedHeapOps :: (Ord a, HG.FixedHeap f a) => f s a -> (f s a -> ST s Bool) -> [FixedHeapOp a] -> ST s Bool
checkFixedHeapOps h check ops =
  let f Nil = pure True
      f (Cons op rest) = liftA2 (&&) rest (applyFixedHeapOp h op >> check h)
   in cata f ops
