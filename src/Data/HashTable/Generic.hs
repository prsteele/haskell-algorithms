module Data.HashTable.Generic where

import qualified Control.Monad as C
import Control.Monad.Primitive
import Control.Monad.ST
import Prelude hiding (mapM_)

type Hash k = k -> Int

type Probe k = Hash k -> k -> Int -> [Int]

class HashTable f k v where
  -- | Query the table for the value associated with a key.
  basicLookup :: f s k v -> k -> ST s (Maybe v)

  -- | Associate a value with a key.
  basicInsert :: f s k v -> k -> v -> ST s ()

  -- | Remove a key and its associated value.
  basicDelete :: f s k v -> k -> ST s ()

  -- | The number of elements stored in the table.
  basicLength :: f s k v -> ST s Int

  -- | The contents of the table.
  basicToList :: f s k v -> ST s [(k, v)]

  -- | The keys of the table.
  basicKeys :: f s k v -> ST s [k]
  basicKeys = fmap (fmap fst) . basicToList

  -- | The values of the table.
  basicValues :: f s k v -> ST s [v]
  basicValues = fmap (fmap snd) . basicToList

linear :: Hash k -> k -> Int -> [Int]
linear h k m = fmap f [0 .. m - 1]
  where
    f i = (h k + i) `mod` m

quadratic :: Int -> Int -> Hash k -> k -> Int -> [Int]
quadratic c1 c2 h k m = fmap f [0 .. m - 1]
  where
    f i = (h k + i * c1 + i * i * c2) `mod` m

{-# INLINE lookup #-}
lookup :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> k -> m (Maybe v)
lookup t k = stToPrim (basicLookup t k)

{-# INLINE insert #-}
insert :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> k -> v -> m ()
insert t k v = stToPrim (basicInsert t k v)

{-# INLINE delete #-}
delete :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> k -> m ()
delete t k = stToPrim (basicDelete t k)

{-# INLINE length #-}
length :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> m Int
length t = stToPrim (basicLength t)

toList :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> m [(k, v)]
toList t = stToPrim (basicToList t)

{-# INLINE keys #-}
keys :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> m [k]
keys t = stToPrim (basicKeys t)

{-# INLINE values #-}
values :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> m [v]
values t = stToPrim (basicValues t)

{-# INLINE mapM_ #-}
mapM_ :: (PrimMonad m, HashTable f k v) => (v -> m ()) -> f (PrimState m) k v -> m ()
mapM_ f t = values t >>= C.mapM_ f

{-# INLINE kmapM_ #-}
kmapM_ :: (PrimMonad m, HashTable f k v) => (k -> v -> m ()) -> f (PrimState m) k v -> m ()
kmapM_ f t = toList t >>= C.mapM_ (uncurry f)

{-# INLINE forM_ #-}
forM_ :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> (v -> m ()) -> m ()
forM_ = flip mapM_

{-# INLINE kforM_ #-}
kforM_ :: (PrimMonad m, HashTable f k v) => f (PrimState m) k v -> (k -> v -> m ()) -> m ()
kforM_ = flip kmapM_
