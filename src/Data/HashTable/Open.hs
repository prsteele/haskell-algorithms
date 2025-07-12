module Data.HashTable.Open
  ( -- * Open-addressed hash tables
    HashTable,

    -- ** Probe functions
    G.linear,
    G.quadratic,

    -- * Construction and destruction
    new,
    toList,
    keys,
    values,

    -- * Insertion, deletion, querying
    insert,
    delete,
    lookup,
    length,

    -- * Folds
    mapM_,
    kmapM_,
    forM_,
    kforM_,
  )
where

import Control.Monad (unless)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Functor.Foldable
import qualified Data.HashTable.Generic as G
import Data.STRef
import qualified Data.Vector.Mutable as M
import Prelude hiding (length, lookup, mapM_)

data Entry k v = Empty | Tombstone | Value (Int, k, v)
  deriving (Eq)

type Hash k = k -> Int

type Probe k = Hash k -> k -> Int -> [Int]

data HashTable s k v = HashTable
  { hash :: Hash k,
    probe :: Probe k,
    size :: STRef s Int,
    vec :: STRef s (M.MVector s (Entry k v))
  }

instance (Eq k) => G.HashTable HashTable k v where
  basicLookup = stLookup
  basicInsert = stInsert
  basicDelete = stDelete
  basicToList = stToList
  basicLength = stLength

stNew :: Hash k -> Probe k -> Int -> ST s (HashTable s k v)
stNew hash probe size =
  HashTable hash probe <$> newSTRef 0 <*> (M.replicate size Empty >>= newSTRef)

stLookup :: (Eq k) => HashTable s k v -> k -> ST s (Maybe v)
stLookup (HashTable hashFun probe _ rv) k =
  let hk = hashFun k

      alg _ Nil = pure Nothing
      alg mv (Cons ix rest) = do
        entry <- M.read mv ix
        case entry of
          Empty -> pure Nothing
          Tombstone -> rest
          Value (hk', k', v) ->
            if hk == hk' && k == k'
              then pure (Just v)
              else rest
   in do
        mv <- readSTRef rv
        cata (alg mv) (probe hashFun k (M.length mv))

stGrow :: (Eq k) => HashTable s k v -> ST s ()
stGrow table@(HashTable _ _ _ rv) = do
  mv <- readSTRef rv
  mv' <- M.replicate (max 1 (M.length mv * 2)) Empty
  writeSTRef rv mv'

  M.forM_ mv $ \case
    Tombstone -> pure ()
    Empty -> pure ()
    Value (_, k, v) -> stInsert table k v

stInsert :: (Eq k) => HashTable s k v -> k -> v -> ST s ()
stInsert table@(HashTable hashFun probe sRef rv) k v =
  let hk = hashFun k

      doInsert mv ix = M.write mv ix (Value (hk, k, v))

      alg _ Nil = pure False
      alg mv (Cons ix rest) = do
        entry <- M.read mv ix
        case entry of
          Value (hk', k', _) ->
            if hk == hk' && k == k'
              then doInsert mv ix >> pure True -- Update an element
              else rest
          _ -> do
            -- Write the entry
            doInsert mv ix

            -- Mark the size increase
            modifySTRef' sRef succ

            -- Note success
            pure True
   in do
        currentSize <- readSTRef sRef
        mv <- readSTRef rv

        let growThenInsert = stGrow table >> stInsert table k v

        if currentSize == M.length mv
          then growThenInsert
          else cata (alg mv) (probe hashFun k (M.length mv)) >>= (`unless` growThenInsert)

stDelete :: (Eq k) => HashTable s k v -> k -> ST s ()
stDelete (HashTable hashFun probe _ rv) k =
  let hk = hashFun k

      alg _ Nil = pure ()
      alg mv (Cons ix rest) = do
        entry <- M.read mv ix
        case entry of
          Empty -> pure ()
          Tombstone -> rest
          Value (hk', k', _) ->
            if hk == hk' && k == k'
              then M.write mv ix Tombstone
              else rest
   in do
        mv <- readSTRef rv
        cata (alg mv) (probe hashFun k (M.length mv))

stToList :: HashTable s k v -> ST s [(k, v)]
stToList (HashTable _ _ _ rv) =
  let f (Value (_, k, v)) rest = (k, v) : rest
      f _ rest = rest
   in readSTRef rv >>= M.foldr f []

stLength :: HashTable s k v -> ST s Int
stLength (HashTable _ _ sRef _) = readSTRef sRef

new :: (PrimMonad m, Eq k) => Hash k -> Probe k -> Int -> m (HashTable (PrimState m) k v)
new h p s = stToPrim (stNew h p s)

{-# INLINE lookup #-}
lookup :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> k -> m (Maybe v)
lookup = G.lookup

{-# INLINE insert #-}
insert :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> k -> v -> m ()
insert = G.insert

{-# INLINE delete #-}
delete :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> k -> m ()
delete = G.delete

{-# INLINE values #-}
values :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> m [v]
values = G.values

{-# INLINE keys #-}
keys :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> m [k]
keys = G.keys

toList :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> m [(k, v)]
toList = G.toList

{-# INLINE length #-}
length :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> m Int
length = G.length

{-# INLINE mapM_ #-}
mapM_ :: (PrimMonad m, Eq k) => (v -> m ()) -> HashTable (PrimState m) k v -> m ()
mapM_ = G.mapM_

{-# INLINE kmapM_ #-}
kmapM_ :: (PrimMonad m, Eq k) => (k -> v -> m ()) -> HashTable (PrimState m) k v -> m ()
kmapM_ = G.kmapM_

{-# INLINE forM_ #-}
forM_ :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> (v -> m ()) -> m ()
forM_ = G.forM_

{-# INLINE kforM_ #-}
kforM_ :: (PrimMonad m, Eq k) => HashTable (PrimState m) k v -> (k -> v -> m ()) -> m ()
kforM_ = G.kforM_
