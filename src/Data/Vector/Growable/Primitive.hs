module Data.Vector.Growable.Primitive
  ( -- * Growable primitive vectors
    GrowMVector (..),

    -- ** Construction
    empty,
    thaw,
    fromMVector,
    fromVector,

    -- *** Destruction
    freeze,

    -- ** Accessors
    length,
    capacity,

    -- ** Growing and shrinking
    append,
    reserve,
    conserve,
    shrink,

    -- ** Accessing individual elements
    read,
    readMaybe,
    write,
    swap,

    -- *** Access the underlying 'MVector'
    withMVector,
    unsafeWithMVector,

    -- ** Maps
    Data.Vector.Growable.Primitive.mapM_,
    imapM_,
    Data.Vector.Growable.Primitive.forM_,
    iforM_,

    -- ** Folds
    foldM',
    Data.Vector.Growable.Primitive.foldM,
    foldl',
    Data.Vector.Growable.Primitive.foldl,
    foldr',
    Data.Vector.Growable.Primitive.foldr,
    foldrM',
    foldrM,
    ifoldM',
    ifoldM,
    ifoldl',
    ifoldl,
    ifoldr',
    ifoldr,
    ifoldrM',
    ifoldrM,
  )
where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive
import Data.STRef
import qualified Data.Vector.Growable as GV
import qualified Data.Vector.Growable.Generic as GG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MP
import Prelude hiding (length, read)

-- | An 'MP.MVector' that can grow (and shrink).
newtype GrowMVector s a = GrowMVector
  { growVectorState :: STRef s (GV.GrowVectorState MP.MVector s a)
  }

instance (Prim a) => GG.GrowVector GrowMVector a where
  type MVector GrowMVector = MP.MVector

  basicEmpty = GV.genericGrowVectorEmpty GrowMVector
  basicFromMVector = GV.genericGrowVectorFromMVector GrowMVector
  basicAppend = GV.genericGrowVectorAppend (.growVectorState)
  basicReserve = GV.genericGrowVectorReserve (.growVectorState)
  basicConserve = GV.genericGrowVectorConserve (.growVectorState)
  basicShrink = GV.genericGrowVectorShrink (.growVectorState)
  basicMVector = GV.genericGrowVectorMVector (.growVectorState)
  basicLength = GV.genericGrowVectorLength (.growVectorState)
  basicCapacity = GV.genericGrowVectorCapacity (.growVectorState)

-- | Create a new growable vector with the given initial capacity.
{-# INLINE empty #-}
empty :: (PrimMonad m, Prim a) => Int -> m (GrowMVector (PrimState m) a)
empty = GG.empty

-- | Create an immutable copy of the grow vector.
{-# INLINE freeze #-}
freeze :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> m (VP.Vector a)
freeze = stToPrim . flip withMVector VP.freeze

-- | Create a growable copy of the vector.
{-# INLINE thaw #-}
thaw :: (PrimMonad m, Prim a) => VP.Vector a -> m (GrowMVector (PrimState m) a)
thaw = stToPrim . (VP.thaw >=> fromMVector)

-- | Create a new growable vector from an immutable vector.
--
-- The mutable vector will be modified in-place; it should no longer
-- be used independently.
{-# INLINE fromMVector #-}
fromMVector :: (PrimMonad m, Prim a) => MP.MVector (PrimState m) a -> m (GrowMVector (PrimState m) a)
fromMVector = GG.fromMVector

-- | Create a new growable vector from an immutable vector.
--
-- This is an alias of 'thaw'.
{-# INLINE fromVector #-}
fromVector :: (PrimMonad m, Prim a) => VP.Vector a -> m (GrowMVector (PrimState m) a)
fromVector = GG.fromVector

-- | Append an element to the vector, growing if necessary.
--
-- This takes \(O(1)\) amortized time. If there is sufficient
-- capacity, this takes \(O(1)\) time; otherwise, this operation takes
-- \(O(n)\) time.
{-# INLINE append #-}
append :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> a -> m ()
append = GG.append

-- | Ensure the capacity of the vector is at least the given size.
--
-- If new space must be reserved, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
{-# INLINE reserve #-}
reserve :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> Int -> m ()
reserve = GG.reserve

-- | Discard excess capacity.
--
-- If there is excess capacity, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
{-# INLINE conserve #-}
conserve :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> m ()
conserve = GG.conserve

-- | Shrink the vector to the given size.
--
-- This is an \(O(n)\) operation, due to cleaning up discarded
-- references.
{-# INLINE shrink #-}
shrink :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> Int -> m ()
shrink = GG.shrink

{-# INLINE length #-}
length :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> m Int
length = GG.length

{-# INLINE withMVector #-}
withMVector :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> (MP.MVector (PrimState m) a -> m b) -> m b
withMVector = GG.withMVector

-- | Access the underlying mutable vector.
--
-- The vector returned is only valid until a call to `append`,
-- `reserve`, or `conserve`; after any of these calls, the
-- `GrowMVector` might be pointing to a new `MVector`.
unsafeWithMVector :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> m (MP.MVector (PrimState m) a)
unsafeWithMVector = GG.unsafeWithMVector

-- | The current capacity of the vector.
--
-- Up to this many elements may be stored in the vector without
-- requiring a resize operation.
{-# INLINE capacity #-}
capacity :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> m Int
capacity = GG.capacity

{-# INLINE read #-}
read :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> Int -> m a
read = GG.read

{-# INLINE readMaybe #-}
readMaybe :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> Int -> m (Maybe a)
readMaybe = GG.readMaybe

{-# INLINE write #-}
write :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> Int -> a -> m ()
write = GG.write

{-# INLINE swap #-}
swap :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> Int -> Int -> m ()
swap = GG.swap

{-# INLINE mapM_ #-}
mapM_ :: (PrimMonad m, Prim a) => (a -> m b) -> GrowMVector (PrimState m) a -> m ()
mapM_ = GG.mapM_

{-# INLINE imapM_ #-}
imapM_ :: (PrimMonad m, Prim a) => (Int -> a -> m b) -> GrowMVector (PrimState m) a -> m ()
imapM_ = GG.imapM_

{-# INLINE forM_ #-}
forM_ :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> (a -> m b) -> m ()
forM_ = GG.forM_

{-# INLINE iforM_ #-}
iforM_ :: (PrimMonad m, Prim a) => GrowMVector (PrimState m) a -> (Int -> a -> m b) -> m ()
iforM_ = GG.iforM_

{-# INLINE foldl #-}
foldl :: (PrimMonad m, Prim a) => (b -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldl = GG.foldl

foldl' :: (PrimMonad m, Prim a) => (b -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldl' = GG.foldl'

{-# INLINE ifoldl #-}
ifoldl :: (PrimMonad m, Prim a) => (b -> Int -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldl = GG.ifoldl

{-# INLINE ifoldl' #-}
ifoldl' :: (PrimMonad m, Prim a) => (b -> Int -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldl' = GG.ifoldl'

{-# INLINE foldM #-}
foldM :: (PrimMonad m, Prim a) => (b -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldM = GG.foldM

{-# INLINE foldM' #-}
foldM' :: (PrimMonad m, Prim a) => (b -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldM' = GG.foldM'

ifoldM :: (PrimMonad m, Prim a) => (b -> Int -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldM = GG.ifoldM

{-# INLINE ifoldM' #-}
ifoldM' :: (PrimMonad m, Prim a) => (b -> Int -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldM' = GG.ifoldM'

{-# INLINE foldr #-}
foldr :: (PrimMonad m, Prim a) => (a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldr = GG.foldr

{-# INLINE foldr' #-}
foldr' :: (PrimMonad m, Prim a) => (a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldr' = GG.foldr'

{-# INLINE ifoldr #-}
ifoldr :: (PrimMonad m, Prim a) => (Int -> a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldr = GG.ifoldr

{-# INLINE ifoldr' #-}
ifoldr' :: (PrimMonad m, Prim a) => (Int -> a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldr' = GG.ifoldr'

{-# INLINE foldrM #-}
foldrM :: (PrimMonad m, Prim a) => (a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldrM = GG.foldrM

{-# INLINE foldrM' #-}
foldrM' :: (PrimMonad m, Prim a) => (a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldrM' = GG.foldrM'

{-# INLINE ifoldrM #-}
ifoldrM :: (PrimMonad m, Prim a) => (Int -> a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldrM = GG.ifoldrM

{-# INLINE ifoldrM' #-}
ifoldrM' :: (PrimMonad m, Prim a) => (Int -> a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldrM' = GG.ifoldrM'
