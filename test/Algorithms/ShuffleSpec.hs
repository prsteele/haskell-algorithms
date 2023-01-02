module Algorithms.ShuffleSpec where

import Algorithms.Shuffle
import Algorithms.Sorting (quicksort)
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "shuffle" $ do
    prop "permutes elements" $ \(IntVector v) seed ->
      quicksort v == quicksort (shuffle seed v)
