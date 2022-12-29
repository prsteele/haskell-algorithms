module Algorithms.Sorting.QuickSortSpec where

import Algorithms.Sorting.QuickSort
import Algorithms.Sorting.TestUtil
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "quickSort" $ do
    prop "is a sorting algorithm" $
      isSortingAlgorithm quickSort . (V.fromList :: [Int] -> V.Vector Int)
    prop "is a sorting algorithm, using pivotLast" $
      isSortingAlgorithm (quickSortWithPivot pivotLast) . (V.fromList :: [Int] -> V.Vector Int)
    prop "is a sorting algorithm, using pivotMedianOf3" $
      isSortingAlgorithm (quickSortWithPivot pivotMedianOf3) . (V.fromList :: [Int] -> V.Vector Int)
