module Algorithms.MaximumSubArraySpec where

import Algorithms.MaximumSubArray
import Algorithms.TestUtil ()
import qualified Data.Vector as V
import Lens.Micro
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "MaximumSubArray" $ do
    prop "finds the maximum sub array" runAssertions

bruteMaximumSubArray :: V.Vector Int -> SubArraySum Int
bruteMaximumSubArray v =
  let subs =
        [ SubArraySum (Slice i j) (V.sum (V.slice i (j - i) v))
          | i <- [0 .. V.length v - 1],
            j <- [i + 1 .. V.length v]
        ]
   in if V.null v
        then SubArraySum (Slice 0 0) 0
        else maximum subs

runAssertions :: V.Vector Int -> Expectation
runAssertions v =
  let maxSub = maximumSubArrayNum v
   in do
        -- Check against the brute-force solution
        (maxSub ^. value) `shouldBe` (bruteMaximumSubArray v ^. value)

        -- Check that the sum agrees with the stated indices
        let i = maxSub ^. slice . lo
            j = maxSub ^. slice . hi
        (maxSub ^. value) `shouldBe` V.sum (V.slice i (j - i) v)
