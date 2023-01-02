module Algorithms.Sorting.Sort3Spec where

import Algorithms.Sorting.Sort3
import Algorithms.Sorting.TestUtil
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "sort3" $ do
    prop "is a stable sorting algorithm" $
      \x y z -> isStableSortingAlgorithm (sortingAdapter sort3) (vec3 x y z)
  describe "sort3By" $ do
    prop "is a stable sorting algorithm" $
      \x y z -> isStableSortingAlgorithm (sortingAdapter (sort3By compare)) (vec3 x y z)
  describe "sort3On" $ do
    prop "is a stable sorting algorithm" $
      \x y z -> isStableSortingAlgorithm (sortingAdapter (sort3On id)) (vec3 x y z)

vec3 :: Int -> Int -> Int -> V.Vector Int
vec3 x y z = V.fromList [x, y, z]

sortingAdapter :: (a -> a -> a -> (a, a, a)) -> V.Vector a -> V.Vector a
sortingAdapter sort v =
  let v3 = V.slice 0 3 v
      (x, y, z) = sort (v3 V.! 0) (v3 V.! 1) (v3 V.! 2)
   in V.fromList [x, y, z]
