module Algorithms.ShuffleSpec where

import Algorithms.Shuffle
import Algorithms.Sorting (quickSort)
import Algorithms.TestUtil
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "shuffle" $ do
    prop "permutes elements" $ \(IntVector v) seed ->
      quickSort v == quickSort (shuffle seed v)
