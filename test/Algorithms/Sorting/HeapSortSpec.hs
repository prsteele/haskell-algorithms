module Algorithms.Sorting.HeapSortSpec where

import Algorithms.Sorting.HeapSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Heap sort" $ do
    prop "is a sorting algorithm" $ \(IntVector v) ->
      isSortingAlgorithm heapsort v
