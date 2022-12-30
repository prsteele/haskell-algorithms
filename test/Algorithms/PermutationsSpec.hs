module Algorithms.PermutationsSpec where

import Algorithms.Permutations
import Algorithms.Shuffle
import Algorithms.TestUtil
import Control.Monad
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (shuffle)
import Prelude hiding (pi)

spec :: Spec
spec = do
  describe "permutationTo" $ do
    prop "finds permutations" checkShuffled
    prop "fails on non-permutations" checkNonPermutations

checkShuffled :: Int -> IntVector -> Expectation
checkShuffled seed (IntVector v) =
  let shuffled = shuffle seed v
      pi = v `permutationTo` shuffled
   in do
        fmap (V.backpermute v) pi `shouldBe` Just shuffled

checkNonPermutations :: IntegerVector -> Property
checkNonPermutations (IntegerVector v) =
  let -- Define the output type to something concrete; since we expect
      -- Nothing, we don't use it, and so type inference fails
      perms :: V.Vector Integer -> V.Vector Integer -> Maybe (V.Vector Int)
      perms = permutationTo
   in (not (V.null v) ==>) $ do
        -- Modifying values defeats permutations
        perms v (fmap succ v) `shouldBe` Nothing
        -- Adding elements defaults permutations
        unless (V.null v) $ perms v (v <> v) `shouldBe` Nothing
