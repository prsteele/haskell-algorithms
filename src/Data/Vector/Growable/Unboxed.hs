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
