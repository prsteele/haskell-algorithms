module Algorithms.Sorting.InsertionSortSpec where

import Algorithms.Sorting.InsertionSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "insertionSort" $ do
    prop "is a stable sorting algorithm" $ \(IntVector v) ->
      isStableSortingAlgorithm insertionSort v
