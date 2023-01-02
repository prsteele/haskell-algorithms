module Algorithms.Sorting.QuicksortSpec where

import Algorithms.Sorting.Quicksort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "quickSort" $ do
    prop "is a sorting algorithm" $ \(IntVector v) ->
      isSortingAlgorithm quicksort v
    prop "is a sorting algorithm, using pivotLast" $ \(IntVector v) ->
      isSortingAlgorithm (quicksortWithPivot pivotLast compare) v
    prop "is a sorting algorithm, using pivotMedianOf3" $ \(IntVector v) ->
      isSortingAlgorithm (quicksortWithPivot (pivotMedianOf3 compare) compare) v
