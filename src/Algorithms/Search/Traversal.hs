{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Traverse a graph in breadth-first or depth-first order.
--
-- Functions in this module use a implicit representation of graphs
-- based on a neighbor function.
module Algorithms.Search.Traversal
  ( -- * Simple traversals
    bfs,
    dfs,

    -- * Effectful traversals
    traverseGraph,

    -- ** Helper types and functions
    TraversalOrder (..),
    Neighbors,
    Effect,
    TraversalResult (..),
    predecessor,
    depth,
    predecessorToTree,
  )
where

import Control.Monad
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Tree
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

data TraversalOrder = BFS | DFS
  deriving (Show)

data SearchState c v = SearchState
  { _pending :: c v,
    _seen :: S.Set v,
    _result :: TraversalResult v
  }

data SearchRules c v m = SearchRules
  { _neighbors :: v -> [v],
    _pushFn :: c v -> v -> c v,
    _popFn :: c v -> Maybe (v, c v),
    _visit :: TraversalResult v -> v -> m ()
  }

-- | Information about the traversal of a graph.
--
-- Lenses for this type are exposed as 'predecessor' and 'depth'.
data TraversalResult v = TraversalResult
  { -- | The predecessor of each discovered node. The source node will
    -- have no predecessor.
    _predecessor :: M.Map v v,
    -- | The depth of each discovered node. The depth of the source
    -- node is zero; this will be the only node with depth zero.
    _depth :: M.Map v Int
  }
  deriving (Show)

makeLenses ''SearchState
makeLenses ''TraversalResult
makeLenses ''SearchRules

-- | The neighbors of a node.
type Neighbors v = v -> [v]

-- | The effectful computation to run when visiting each node.
--
-- This effect will be evaluated once per discovered node, when the
-- node is first discovered.
type Effect v m = TraversalResult v -> v -> m ()

type SearchM c v m a = StateT (SearchState c v) (ReaderT (SearchRules c v m) m) a

-- | Traverse a graph, performing an effectful computation at each node visited.
--
-- Traverse a graph in either breadth-first or depth-first order. The
-- first time each node is visited, the provided effectful computation
-- is run.
--
-- The 'bfs' and 'dfs' functions simply call this function with @const
-- (const (pure ()))@ as the effect in the 'Identity' Monad.
traverseGraph :: (Monad m, Ord v) => TraversalOrder -> Neighbors v -> Effect v m -> v -> m (TraversalResult v)
traverseGraph BFS neighborF visitF source = evalSearchM' traverseGraph' rules initState
  where
    pop' c = case c of
      Seq.Empty -> Nothing
      x Seq.:<| xs -> Just (x, xs)
    push' = (Seq.:|>)
    rules = mkSearchRules push' pop' neighborF visitF
    initState = mkSearchState pure source
traverseGraph DFS neighborF visitF source = evalSearchM' traverseGraph' rules initState
  where
    pop' c = case c of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
    push' = flip (:)
    rules = mkSearchRules push' pop' neighborF visitF
    initState = mkSearchState pure source

-- | Traverse a graph in breadth-first order.
--
-- This traverses the graph in breadth-first order, returning the
-- breadth-first search tree (via the predecessor graph) and depth
-- information about each node.
bfs :: Ord v => Neighbors v -> v -> TraversalResult v
bfs neighborsF source = runIdentity (traverseGraph BFS neighborsF (\_ _ -> pure ()) source)

-- | Traverse a graph in depth-first order.
--
-- This traverses the graph in depth-first order, returning the
-- depth-first search tree (via the predecessor graph) and depth
-- information about each node.
dfs :: Ord v => Neighbors v -> v -> TraversalResult v
dfs neighborsF source = runIdentity (traverseGraph DFS neighborsF (\_ _ -> pure ()) source)

mkSearchRules :: (c v -> v -> c v) -> (c v -> Maybe (v, c v)) -> (v -> [v]) -> Effect v m -> SearchRules c v m
mkSearchRules push' pop' neighborsF visitF =
  SearchRules
    { _neighbors = neighborsF,
      _pushFn = push',
      _popFn = pop',
      _visit = visitF
    }

mkSearchState :: (v -> c v) -> v -> SearchState c v
mkSearchState singleton' source =
  SearchState
    { _pending = singleton' source,
      _seen = S.singleton source,
      _result =
        TraversalResult
          { _predecessor = M.empty,
            _depth = M.singleton source 0
          }
    }

evalSearchM' :: (Monad m, Ord v) => SearchM c v m a -> SearchRules c v m -> SearchState c v -> m (TraversalResult v)
evalSearchM' effect rules initState = fmap (^. result) finalState
  where
    finalState = runReaderT (execStateT effect initState) rules

pop :: Monad m => SearchM c v m (Maybe (v, c v))
pop = view popFn <*> use pending

push :: Monad m => v -> SearchM c v m ()
push new = do
  pushF <- view pushFn
  modifying pending (`pushF` new)

traverseGraph' :: (Monad m, Ord v) => SearchM c v m ()
traverseGraph' = do
  mNext <- pop
  case mNext of
    Nothing -> pure ()
    Just (next, rest) -> do
      assign pending rest
      explore next
      traverseGraph'

explore :: (Monad m, Ord v) => v -> SearchM c v m ()
explore current = do
  -- Get the current depth
  currentDepth <- fmap (M.findWithDefault 0 current) (use (result . depth))

  -- Explore each neighbor
  neighboring <- view neighbors <*> pure current
  forM_ neighboring $ \neighbor -> do
    observed <- use seen
    unless (neighbor `S.member` observed) $ do
      -- Visit the neighbor
      visitF <- view visit
      currentResult <- use result
      lift (lift (visitF currentResult neighbor))

      -- Mark the neighbor as seen
      modifying seen (S.insert neighbor)

      -- Enqueue the neighbor
      push neighbor

      -- Record the depth and predecessor of the neighbor
      modifying (result . predecessor) (M.insert neighbor current)
      modifying (result . depth) (M.insert neighbor (currentDepth + 1))

-- | Invert a predecessor graph into a spanning tree over the
-- discovered nodes.
--
-- In a well-formed predecessor graph describing a search from a
-- single source node, the resulting forest will have a single tree.
predecessorToTree :: Ord v => M.Map v v -> [Tree v]
predecessorToTree preds =
  let backward = M.toList preds

      -- There should only be one root, but
      roots = S.fromList parents `S.difference` S.fromList children
        where
          parents = fmap snd backward
          children = fmap fst backward

      -- The children of each node
      forward = go M.empty backward
        where
          go tree [] = tree
          go tree ((child, parent) : rest) = go tree' rest
            where
              tree' = case M.lookup parent tree of
                Nothing -> M.insert parent [child] tree
                Just children -> M.insert parent (child : children) tree

      f root = Node root (fmap f (M.findWithDefault [] root forward))
   in fmap f (S.toList roots)
