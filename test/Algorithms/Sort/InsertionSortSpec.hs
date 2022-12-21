module Algorithms.Sort.InsertionSortSpec where

import Algorithms.Sort.InsertionSort
import Algorithms.Sort.TestUtil
import Control.Monad
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Insertion sort" $ do
    -- Manual test cases
    forM_ manualCases $ \values -> do
      it ("sorts " <> show values) $ runAssertions values

    -- Arbitrary test cases
    prop "sorts arbitary lists" $ runAssertions . V.fromList

runAssertions :: V.Vector Int -> Expectation
runAssertions v = insertionSort v `assertIsSortedCopyOf` v

manualCases :: [V.Vector Int]
manualCases =
  fmap
    V.fromList
    [ [],
      [1 .. 5],
      [5, 4 .. 1]
    ]
