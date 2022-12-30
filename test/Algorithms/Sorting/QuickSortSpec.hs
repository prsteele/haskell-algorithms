module Algorithms.Sorting.QuickSortSpec where

import Algorithms.Sorting.QuickSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "quickSort" $ do
    prop "is a sorting algorithm" $ \(IntVector v) ->
      isSortingAlgorithm quickSort v
    prop "is a sorting algorithm, using pivotLast" $ \(IntVector v) ->
      isSortingAlgorithm (quickSortWithPivot pivotLast) v
    prop "is a sorting algorithm, using pivotMedianOf3" $ \(IntVector v) ->
      isSortingAlgorithm (quickSortWithPivot pivotMedianOf3) v
