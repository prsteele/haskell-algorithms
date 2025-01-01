module Algorithms.ShortestPaths.DijkstraSpec where

import Algorithms.ShortestPaths.Dijkstra
import Algorithms.TestUtil
import Algorithms.Utility
import Control.Monad
import Data.Bifunctor
import qualified Data.Map.Strict as M
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  describe "Dijkstra's algorithm" $ do
    prop "works on Manhattan distances" dijkstrasManhattan
    prop "works on line graphs" dijkstrasQuadraticLine
    prop "halts on negative cycles" dijkstrasNegativeCostCycle

manhattanCost :: (Int, Int) -> (Int, Int) -> Int
manhattanCost (x, y) (x', y') = abs (x - x') + abs (y - y')

manhattanGridNeighbors :: Int -> (Int, Int) -> [(Int, Int)]
manhattanGridNeighbors width (x, y) = filter inBounds [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    inBounds = uncurry (&&) . dup bimap (between 0 width)

(==?) :: (Monad m, Show a, Eq a) => a -> a -> PropertyM m ()
x ==? y =
  if x == y
    then assertWith True (show x <> " == " <> show y)
    else assertWith False (show x <> " /= " <> show y)

dijkstrasManhattan :: Positive Int -> (Int, Int) -> (Int, Int) -> Property
dijkstrasManhattan (Positive width) source' sink' =
  let mkValid = bimap f f
        where
          f = (`mod` (width + 1))

      -- Map the source and sink to valid indices
      source = mkValid source'
      sink = mkValid sink'

      adjacent u v = manhattanCost u v == 1
   in monadicIO $ do
        case dijkstraPath manhattanCost (manhattanGridNeighbors width) source sink of
          Nothing ->
            assertWith False "to find the sink"
          Just (path, actual) -> do
            -- Is the cost correct?
            manhattanCost source sink ==? actual

            -- Do we start at the source?
            case path of
              x : _ -> source ==? x
              _ -> failure "empty path"

            -- Do we end at the sink?
            case reverse path of
              x : _ -> assertWith (x == sink) (show path) -- sink ==? x
              _ -> failure "empty path"

            -- Are all path elements adjacent?
            forM_ (zip path (tail path)) $ \(u, v) ->
              assertWith (adjacent u v) (show u <> " not adjacent to " <> show v)

quadraticLineCost :: Int -> Int -> Int
quadraticLineCost x y = (x - y) ^ (2 :: Int)

lineNeighbors :: Int -> Int -> [Int]
lineNeighbors nodes = const [0 .. nodes - 1]

dijkstrasQuadraticLine :: Positive Int -> Int -> Int -> Property
dijkstrasQuadraticLine (Positive nodes) source' sink' =
  let mkValid = (`mod` nodes)
      source = mkValid source'
      sink = mkValid sink'
   in monadicIO $ do
        case dijkstraPath quadraticLineCost (lineNeighbors nodes) source sink of
          Nothing ->
            failure "to find the sink"
          Just (path, actual) -> do
            -- Is the cost correct?
            abs (sink - source) ==? actual

            -- Do we start at the source?
            case path of
              x : _ -> source ==? x
              _ -> failure "empty path"

            -- Do we end at the sink?
            case reverse path of
              x : _ -> assertWith (x == sink) (show path) -- sink ==? x
              _ -> failure "empty path"

dijkstrasNegativeCostCycle :: Property
dijkstrasNegativeCostCycle =
  let nodes = [0 .. 4] :: [Int]
      tree = dijkstraTree (\_ _ -> (-1 :: Int)) (const nodes) 0
   in monadicIO $
        forM_ nodes $ \u ->
          assertWith (M.member u tree) ("to find " <> show u)
