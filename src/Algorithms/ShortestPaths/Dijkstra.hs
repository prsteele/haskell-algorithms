module Algorithms.ShortestPaths.Dijkstra where

import Control.Monad
import Control.Monad.ST
import qualified Data.Heap.Intrusive as HI
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Ord
import Data.STRef
import Data.Semigroup
import qualified Data.Vector.Growable as GV

-- | The shortest path from a source to a sink.
dijkstraPath :: (Ord a, Ord b, Num b) => (a -> a -> b) -> (a -> [a]) -> a -> a -> Maybe ([a], b)
dijkstraPath cost neighbors source sink =
  treeToPath source sink $ dijkstra cost neighbors source (Just sink)

-- | The single-source shortest paths tree.
dijkstraTree :: (Ord a, Ord b, Num b) => (a -> a -> b) -> (a -> [a]) -> a -> M.Map a (a, b)
dijkstraTree cost neighbors source = dijkstra cost neighbors source Nothing

-- | Single-source shortest paths.
dijkstra :: (Ord a, Ord b, Num b) => (a -> a -> b) -> (a -> [a]) -> a -> Maybe a -> M.Map a (a, b)
dijkstra cost neighbors source sink = runST $ do
  -- The priority queue of nodes to explore
  queue <- GV.empty 1 >>= HI.heap compare
  -- Callbacks to adjust node priority
  handles <- newSTRef M.empty
  -- The parents and costs of discovered nodes
  parentAndCosts <- newSTRef M.empty

  let enqueue u c = HI.push' queue (Down (Arg c u)) >>= modifySTRef handles . M.insert u

  let updatePath v p = do
        modifySTRef parentAndCosts (M.insert v p)

        let c_sv = snd p
        caseM (M.lookup v <$> readSTRef handles) $ \case
          Nothing ->
            -- v is not in the queue
            enqueue v c_sv
          Just h ->
            -- v is in the queue (or has already been processed), reprioritize
            h (Down (Arg c_sv v))

  let relax (u, c_su) v = do
        let c_sv' = c_su + cost u v

        improvement <-
          caseM (M.lookup v <$> readSTRef parentAndCosts) $
            pure . \case
              Nothing -> True
              Just (_, c_sv) -> c_sv' < c_sv

        when improvement (updatePath v (u, c_sv'))

  -- The source is trivially reachable
  updatePath source (source, 0)

  -- Process every node in the queue
  whileM (HI.pop queue) $ \(Down (Arg c_su u)) ->
    let foundSink = Just u == sink
     in if foundSink
          then -- Early exit
            HI.clear queue
          else -- Relax neighbors
            forM_ (neighbors u) (relax (u, c_su))

  readSTRef parentAndCosts

-- | Extract a shortest path from a shortest path tree.
treeToPath :: (Ord a, Num b) => a -> a -> M.Map a (a, b) -> Maybe ([a], b)
treeToPath source sink parents = f <$> go sink
  where
    f xs = (fmap fst (reverse (NE.toList xs)), snd (NE.head xs))

    go v
      | source == v = Just (NE.singleton (source, 0))
      | otherwise = case M.lookup v parents of
          Nothing -> Nothing
          Just (p, c) -> ((v, c) NE.<|) <$> go p

caseM :: (Monad m) => m a -> (a -> m b) -> m b
caseM = (>>=)

whileM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whileM next action = go
  where
    go = next >>= maybe (pure ()) (action >=> const go)
