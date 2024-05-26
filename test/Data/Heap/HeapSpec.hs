{-# LANGUAGE TypeOperators #-}
module Data.Heap.HeapSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad.Primitive
import qualified Data.Heap as H
import qualified Data.Heap.Generic as HG
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Growable as GV
import Data.Proxy

spec :: Spec
spec = do
  describe "indexing operations" $ do
    prop "parent . left = id" $ \(NonNegative i) -> H.parent (H.left i) == i
    prop "parent . right = id" $ \(NonNegative i) -> H.parent (H.right i) == i
    prop "parent i < left i < right i" $ \(NonNegative i) ->
      H.parent i < H.left i && H.left i < H.right i
  describe "heap invariant" $ do
    prop "popped element greatest" poppedGreatest

pushAll :: (HG.Heap f a, Ord a) => f s a -> [a] -> ST s ()
pushAll h = mapM_ (HG.heapPush h)

make :: ST s (H.Heap (GV.GrowVector MV.MVector) s a)
make = do
  mv <- MV.new 0
  gv <- GV.fromMVector mv
  pure (H.Heap gv)

heap :: Proxy v -> Proxy (H.Heap v)
heap _ = Proxy

make' :: Ord a => ST s (H.Heap (GV.GrowVector MV.MVector) s a)
make' = HG.new (heap (GV.growVector GV.mvector)) 0

poppedGreatest :: [Int] -> Bool
poppedGreatest xs =
  let
    greatest = case xs of
      [] -> Nothing
      _ -> Just (maximum xs)
  in runST $ do
    h <- HG.new (heap (GV.growVector GV.mvector)) 0
    pushAll h xs
    x <- HG.heapPop h
    pure (x == greatest)
