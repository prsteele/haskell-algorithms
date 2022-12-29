module Algorithms.Sorting.QuickSortSpec where

import Algorithms.Sorting.QuickSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil ()
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "quickSort" $ do
    prop "is a sorting algorithm" $ \v ->
      isSortingAlgorithm quickSort (v :: V.Vector Int)
    prop "is a sorting algorithm, using pivotLast" $ \v ->
      isSortingAlgorithm (quickSortWithPivot pivotLast) (v :: V.Vector Int)
    prop "is a sorting algorithm, using pivotMedianOf3" $ \v ->
      isSortingAlgorithm (quickSortWithPivot pivotMedianOf3) (v :: V.Vector Int)
