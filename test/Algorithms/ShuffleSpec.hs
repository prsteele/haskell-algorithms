module Algorithms.ShuffleSpec where

import Algorithms.Shuffle
import Algorithms.Sorting (quickSort)
import Algorithms.TestUtil ()
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "shuffle" $ do
    prop "permutes elements" $ \v seed ->
      quickSort (v :: V.Vector Int) == quickSort (shuffle seed v)
