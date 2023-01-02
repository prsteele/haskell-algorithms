module Algorithms.Sorting.MergeSortSpec where

import Algorithms.Sorting.MergeSort
import Algorithms.Sorting.Quicksort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "mergeSort" $ do
    prop "is a stable sorting algorithm" $ \(IntVector v) ->
      isStableSortingAlgorithm mergeSort v
  describe "mergeSortOn" $ do
    prop "is a stable sorting algorithm" $ \(IntVector v) ->
      isStableSortingAlgorithm (mergeSortOn id) v
  describe "mergeSortBy" $ do
    prop "is a stable sorting algorithm" $ \(IntVector v) ->
      isStableSortingAlgorithm (mergeSortBy compare) v
  describe "mutMerge" $ do
    prop "properly merges vectors" $ \(IntVector v) (IntVector w) ->
      merge compare (quicksort v) (quicksort w) `shouldBe` quicksort (v <> w)
