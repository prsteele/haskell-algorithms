module Algorithms.Search.TraversalSpec where

import Algorithms.Search.Traversal
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import Data.Tuple (swap)
import Lens.Micro
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "bfs" $ do
    prop "traverses a line graph" $ \(NonNegative n) (NonNegative source) ->
      let neighbors 0 = [1 | n > 0]
          neighbors i
            | i == n = [n - 1 | n > 0]
            | otherwise = [i - 1, i + 1]

          source' = source `mod` (n + 1)

          result = bfs neighbors source'

          correctPredecessor :: Int -> Maybe Int
          correctPredecessor i
            | i < source' = Just (i + 1)
            | i > source' = Just (i - 1)
            | otherwise = Nothing

          correctDepth i = abs (i - source')
       in do
            forM_ [0 .. n] $ \i -> do
              result ^. predecessor . to (M.lookup i) `shouldBe` correctPredecessor i
              result ^. depth . to (M.lookup i) `shouldBe` Just (correctDepth i)

  describe "bfs and dfs" $ do
    prop "discover the same nodes on arbitrary graphs" $ \(ArbitraryNeighbors adj) ->
      let neighbors k = M.findWithDefault [] k adj
          bfsResult = bfs neighbors 0
          dfsResult = dfs neighbors 0

          bfsNodeSet = bfsResult ^. predecessor . to M.keys . to S.fromList
          dfsNodeSet = dfsResult ^. predecessor . to M.keys . to S.fromList
       in bfsNodeSet `shouldBe` dfsNodeSet

  describe "predecessorToTree" $ do
    prop "inverts the predecessor graph" $ \(ArbitraryNeighbors adj) ->
      let neighbors k = M.findWithDefault [] k adj
          bfsResult = bfs neighbors 0
          forest = predecessorToTree (bfsResult ^. predecessor)

          -- Flatten the tree to a list of edges
          edges = concatMap treeEdges forest
       in M.fromList (fmap swap edges) `shouldBe` (bfsResult ^. predecessor)

treeEdges :: Tree a -> [(a, a)]
treeEdges (Node root children) = edges ++ concatMap treeEdges children
  where
    edges = [(root, child) | Node child _ <- children]

newtype ArbitraryNeighbors = ArbitraryNeighbors (M.Map Int [Int])
  deriving (Show)

instance Arbitrary ArbitraryNeighbors where
  arbitrary = do
    NonNegative n <- arbitrary

    let nodes = [0 .. n]

    adjacencies <- replicateM (length nodes) (arbitrarySubset nodes)

    let adj = M.fromList (zip nodes adjacencies)

    pure (ArbitraryNeighbors adj)

arbitrarySubset :: [a] -> Gen [a]
arbitrarySubset xs = do
  marks <- replicateM (length xs) arbitrary
  pure . fmap snd . filter fst $ zip marks xs
