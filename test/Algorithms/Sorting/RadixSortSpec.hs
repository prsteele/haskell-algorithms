{-# LANGUAGE FlexibleContexts #-}

module Algorithms.Sorting.RadixSortSpec where

import Algorithms.Sorting.MergeSort
import Algorithms.Sorting.RadixSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "twosComplementRadixSort" $
    prop "is a sorting algorithm" $ \(IntVector v) ->
      isSortingAlgorithm twosComplementRadixSort v
  describe "radixSort" $
    prop "is a stable sorting algorithm" $ \(IntVector v) ->
      let v' = V.map (uncurry Ix) (V.indexed v)
          sort radix = radixSort radix mutMergeSortOn (finiteBitSize (undefined :: Int))
       in do
            -- We inline isStableSortingAlgorithm since we need to change
            -- our radix function to run the stability tests
            isSortingAlgorithm (sort twosComplementRadix) v
            isStablySorted (sort (twosComplementRadix . _value) v')
  describe "exampleTupleRadixSort" $
    prop "is a sorting algorithm" $
      isSortingAlgorithm exampleTupleRadixSort . V.fromList
