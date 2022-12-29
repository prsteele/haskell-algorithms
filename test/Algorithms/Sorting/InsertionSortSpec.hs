module Algorithms.Sorting.InsertionSortSpec where

import Algorithms.Sorting.InsertionSort
import Algorithms.Sorting.TestUtil
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "insertionSort" $ do
    prop "is a stable sorting algorithm" $
      isStableSortingAlgorithm insertionSort . (V.fromList :: [Int] -> V.Vector Int)
