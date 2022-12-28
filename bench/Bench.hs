{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Algorithms.MaximumSubArray
import Algorithms.Sorting.InsertionSort
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import qualified Data.Vector as V
import System.Random
import System.Random.Stateful

randomVec :: StatefulGen g IO => Int -> g -> IO (V.Vector Double)
randomVec n = fmap V.fromList . replicateM n . uniformRM (-1e10, 1e10)

genSizes :: Int -> [Int]
genSizes = fmap (2 ^) . enumFromTo 0

orderedTestCases :: [Int] -> [V.Vector Double]
orderedTestCases sizes = [V.fromList (fmap fromIntegral [1 .. size]) | size <- sizes]

reversedTestCases :: [Int] -> [V.Vector Double]
reversedTestCases = fmap V.reverse . orderedTestCases

randomTestCases :: Int -> [Int] -> IO [V.Vector Double]
randomTestCases seed sizes = do
  g <- newIOGenM (mkStdGen seed)

  forM sizes $ \size -> do
    randomVec size g

mkInsertionSortBenchmarks :: IO [Benchmark]
mkInsertionSortBenchmarks =
  let insertionSortSizes = genSizes 10

      mkBench v = bench (show (V.length v)) (nf insertionSort v)
   in do
        randomCases <- randomTestCases 0 insertionSortSizes
        pure
          [ bgroup
              "insertionSort (ordered)"
              (fmap mkBench (orderedTestCases insertionSortSizes)),
            bgroup
              "insertionSort (reversed)"
              (fmap mkBench (reversedTestCases insertionSortSizes)),
            bgroup
              "insertionSort (random)"
              (fmap mkBench randomCases)
          ]

instance NFData Slice where
  rnf (Slice i j) = seq i (seq j ())

instance NFData a => NFData (SubArraySum a) where
  rnf (SubArraySum x y) = seq x (seq y ())

mkMaximumSubArrayBenchmarks :: IO [Benchmark]
mkMaximumSubArrayBenchmarks =
  let mkBench v = bench (show (V.length v)) (nf maximumSubArrayNum v)
   in do
        randomCases <- randomTestCases 0 (genSizes 20)

        pure
          [ bgroup
              "maximumSubArray (random)"
              (fmap mkBench randomCases)
          ]

main :: IO ()
main =
  let groups =
        [ mkInsertionSortBenchmarks,
          mkMaximumSubArrayBenchmarks
        ]
   in do
        benchmarks <- fmap concat (sequence groups)
        defaultMain benchmarks
