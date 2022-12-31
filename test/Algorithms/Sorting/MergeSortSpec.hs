module Algorithms.Sorting.MergeSortSpec where

import Algorithms.Sorting.MergeSort
import Algorithms.Sorting.QuickSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "mergeSort" $ do
    prop "is a stable sorting algorithm" $ \(IntVector v) ->
      isStableSortingAlgorithm mergeSort (v :: V.Vector Int)
  describe "mutMerge" $ do
    prop "properly merges vectors" $ \(IntVector v) (IntVector w) ->
      merge (quickSort v) (quickSort w) `shouldBe` quickSort (v <> w)