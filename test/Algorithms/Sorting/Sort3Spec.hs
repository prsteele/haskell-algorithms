module Algorithms.Sorting.Sort3Spec where

import Algorithms.Sorting.Sort3
import Algorithms.Sorting.TestUtil
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "sort3" $ do
    prop "sorts arbitrary triples" checkSort3
    prop "is a stable sorting algorithm" $
      \x y z -> isStableSortingAlgorithm sortingAdapter (V.fromList [x, y, z :: Int])

sortingAdapter :: Ord a => V.Vector a -> V.Vector a
sortingAdapter v =
  let v3 = V.slice 0 3 v
      (x, y, z) = sort3 (v3 V.! 0) (v3 V.! 1) (v3 V.! 2)
   in V.fromList [x, y, z]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

checkSort3 :: Int -> Int -> Int -> Bool
checkSort3 x y z =
  let check (x', y', z') = x' <= y' && y' <= z'
   in all
        (check . uncurry3 sort3)
        [ (x, y, z),
          (x, z, y),
          (y, x, z),
          (y, z, x),
          (z, x, y),
          (z, y, x)
        ]
