{-# LANGUAGE FlexibleContexts #-}

import Algorithms.Sort.InsertionSort
import Control.Monad
import Criterion.Main
import qualified Data.Vector as V
import System.Random
import System.Random.Stateful

randomVec :: StatefulGen g IO => Int -> g -> IO (V.Vector Double)
randomVec n = fmap V.fromList . replicateM n . uniformRM (-1e100, 1e100)

genSizes :: Int -> [Int]
genSizes = fmap (10 ^) . enumFromTo 0

orderedTestCases :: [Int] -> [V.Vector Double]
orderedTestCases sizes = [V.fromList (fmap fromIntegral [1 .. size]) | size <- sizes]

reversedTestCases :: [Int] -> [V.Vector Double]
reversedTestCases = fmap V.reverse . orderedTestCases

randomTestCases :: Int -> [Int] -> IO [V.Vector Double]
randomTestCases seed sizes = do
  g <- newIOGenM (mkStdGen seed)

  forM sizes $ \size -> do
    randomVec size g

main :: IO ()
main =
  let insertionSortSizes = genSizes 3

      mkInsertionSortBench v = bench (show (V.length v)) (nf insertionSort v)
   in do
        randomCases <- randomTestCases 0 insertionSortSizes
        defaultMain
          [ bgroup
              "insertionSort (ordered)"
              (fmap mkInsertionSortBench (orderedTestCases insertionSortSizes)),
            bgroup
              "insertionSort (reversed)"
              (fmap mkInsertionSortBench (reversedTestCases insertionSortSizes)),
            bgroup
              "insertionSort (random)"
              (fmap mkInsertionSortBench randomCases)
          ]
