module Data.Vector.Growable.Unboxed
  ( -- * Growable unboxed vectors
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
    Data.Vector.Growable.Unboxed.mapM_,
    imapM_,
    Data.Vector.Growable.Unboxed.forM_,
    iforM_,

    -- ** Folds
    foldM',
    Data.Vector.Growable.Unboxed.foldM,
    foldl',
    Data.Vector.Growable.Unboxed.foldl,
    foldr',
    Data.Vector.Growable.Unboxed.foldr,
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
import Data.STRef
import qualified Data.Vector.Growable as GV
import qualified Data.Vector.Growable.Generic as GG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (length, read)

-- | An 'MU.MVector' that can grow (and shrink).
newtype GrowMVector s a = GrowMVector
  { growVectorState :: STRef s (GV.GrowVectorState MU.MVector s a)
  }

instance (MU.Unbox a) => GG.GrowVector GrowMVector a where
  type MVector GrowMVector = MU.MVector

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
empty :: (PrimMonad m, MU.Unbox a) => Int -> m (GrowMVector (PrimState m) a)
empty = GG.empty

-- | Create an immutable copy of the grow vector.
freeze :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> m (VU.Vector a)
freeze = stToPrim . flip withMVector VU.freeze

-- | Create a growable copy of the vector.
thaw :: (PrimMonad m, MU.Unbox a) => VU.Vector a -> m (GrowMVector (PrimState m) a)
thaw = stToPrim . (VU.thaw >=> fromMVector)

-- | Create a new growable vector from an immutable vector.
--
-- The mutable vector will be modified in-place; it should no longer
-- be used independently.
fromMVector :: (PrimMonad m, MU.Unbox a) => MU.MVector (PrimState m) a -> m (GrowMVector (PrimState m) a)
fromMVector = GG.fromMVector

-- | Create a new growable vector from an immutable vector.
--
-- This is an alias of 'thaw'.
fromVector :: (PrimMonad m, MU.Unbox a) => VU.Vector a -> m (GrowMVector (PrimState m) a)
fromVector = GG.fromVector

-- | Append an element to the vector, growing if necessary.
--
-- This takes \(O(1)\) amortized time. If there is sufficient
-- capacity, this takes \(O(1)\) time; otherwise, this operation takes
-- \(O(n)\) time.
append :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> a -> m ()
append = GG.append

-- | Ensure the capacity of the vector is at least the given size.
--
-- If new space must be reserved, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
reserve :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> Int -> m ()
reserve = GG.reserve

-- | Discard excess capacity.
--
-- If there is excess capacity, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
conserve :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> m ()
conserve = GG.conserve

-- | Shrink the vector to the given size.
--
-- This is an \(O(n)\) operation, due to cleaning up discarded
-- references.
shrink :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> Int -> m ()
shrink = GG.shrink

length :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> m Int
length = GG.length

withMVector :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> (MU.MVector (PrimState m) a -> m b) -> m b
withMVector = GG.withMVector

-- | Access the underlying mutable vector.
--
-- The vector returned is only valid until a call to `append`,
-- `reserve`, or `conserve`; after any of these calls, the
-- `GrowMVector` might be pointing to a new `MVector`.
unsafeWithMVector :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> m (MU.MVector (PrimState m) a)
unsafeWithMVector = GG.unsafeWithMVector

-- | The current capacity of the vector.
--
-- Up to this many elements may be stored in the vector without
-- requiring a resize operation.
capacity :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> m Int
capacity = GG.capacity

read :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> Int -> m a
read = GG.read

readMaybe :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> Int -> m (Maybe a)
readMaybe = GG.readMaybe

write :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> Int -> a -> m ()
write = GG.write

swap :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> Int -> Int -> m ()
swap = GG.swap

mapM_ :: (PrimMonad m, MU.Unbox a) => (a -> m b) -> GrowMVector (PrimState m) a -> m ()
mapM_ = GG.mapM_

imapM_ :: (PrimMonad m, MU.Unbox a) => (Int -> a -> m b) -> GrowMVector (PrimState m) a -> m ()
imapM_ = GG.imapM_

forM_ :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> (a -> m b) -> m ()
forM_ = GG.forM_

iforM_ :: (PrimMonad m, MU.Unbox a) => GrowMVector (PrimState m) a -> (Int -> a -> m b) -> m ()
iforM_ = GG.iforM_

foldl :: (PrimMonad m, MU.Unbox a) => (b -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldl = GG.foldl

foldl' :: (PrimMonad m, MU.Unbox a) => (b -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldl' = GG.foldl'

ifoldl :: (PrimMonad m, MU.Unbox a) => (b -> Int -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldl = GG.ifoldl

ifoldl' :: (PrimMonad m, MU.Unbox a) => (b -> Int -> a -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldl' = GG.ifoldl'

foldM :: (PrimMonad m, MU.Unbox a) => (b -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldM = GG.foldM

foldM' :: (PrimMonad m, MU.Unbox a) => (b -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldM' = GG.foldM'

ifoldM :: (PrimMonad m, MU.Unbox a) => (b -> Int -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldM = GG.ifoldM

ifoldM' :: (PrimMonad m, MU.Unbox a) => (b -> Int -> a -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldM' = GG.ifoldM'

foldr :: (PrimMonad m, MU.Unbox a) => (a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldr = GG.foldr

foldr' :: (PrimMonad m, MU.Unbox a) => (a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
foldr' = GG.foldr'

ifoldr :: (PrimMonad m, MU.Unbox a) => (Int -> a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldr = GG.ifoldr

ifoldr' :: (PrimMonad m, MU.Unbox a) => (Int -> a -> b -> b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldr' = GG.ifoldr'

foldrM :: (PrimMonad m, MU.Unbox a) => (a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldrM = GG.foldrM

foldrM' :: (PrimMonad m, MU.Unbox a) => (a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
foldrM' = GG.foldrM'

ifoldrM :: (PrimMonad m, MU.Unbox a) => (Int -> a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldrM = GG.ifoldrM

ifoldrM' :: (PrimMonad m, MU.Unbox a) => (Int -> a -> b -> m b) -> b -> GrowMVector (PrimState m) a -> m b
ifoldrM' = GG.ifoldrM'
