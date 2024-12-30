-- | Mutable vectors that can grow (and shrink.)
module Data.Vector.Growable
  ( -- * Growable vectors
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

    -- * Helper functions
    GrowVectorState (..),
    genericGrowVectorEmpty,
    genericGrowVectorAppend,
    genericGrowVectorLength,
    genericGrowVectorShrink,
    genericGrowVectorMVector,
    genericGrowVectorReserve,
    genericGrowVectorCapacity,
    genericGrowVectorConserve,
    genericGrowVectorFromMVector,
  )
where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Growable.Generic as GG
import qualified Data.Vector.Mutable as MV
import Prelude hiding (length, read)

-- | An 'MVector' that can grow (and shrink).
newtype GrowMVector s a = GrowMVector
  { growVectorState :: STRef s (GrowVectorState MV.MVector s a)
  }

data GrowVectorState v s a = GrowVectorState
  { _growVectorStateVector :: v s a,
    _growVectorStateSize :: Int,
    _growVectorStateCapacity :: Int
  }

instance GG.GrowVector GrowMVector a where
  type MVector GrowMVector = MV.MVector

  basicEmpty = genericGrowVectorEmpty GrowMVector
  basicFromMVector = genericGrowVectorFromMVector GrowMVector
  basicAppend = genericGrowVectorAppend (.growVectorState)
  basicReserve = genericGrowVectorReserve (.growVectorState)
  basicConserve = genericGrowVectorConserve (.growVectorState)
  basicShrink = genericGrowVectorShrink (.growVectorState)
  basicMVector = genericGrowVectorMVector (.growVectorState)
  basicLength = genericGrowVectorLength (.growVectorState)
  basicCapacity = genericGrowVectorCapacity (.growVectorState)

-- | Create an empty growable vector with the given initial capacity.
empty :: (PrimMonad m) => Int -> m (GrowMVector (PrimState m) a)
empty = GG.empty

-- | Create an immutable copy of the grow vector.
freeze :: (PrimMonad m) => GrowMVector (PrimState m) a -> m (V.Vector a)
freeze = stToPrim . flip withMVector V.freeze

-- | Create a growable copy of the vector.
thaw :: (PrimMonad m) => V.Vector a -> m (GrowMVector (PrimState m) a)
thaw = stToPrim . (V.thaw >=> fromMVector)

-- | Create a new growable vector from an immutable vector.
--
-- The mutable vector will be modified in-place; it should no longer
-- be used independently.
fromMVector :: (PrimMonad m) => MV.MVector (PrimState m) a -> m (GrowMVector (PrimState m) a)
fromMVector = GG.fromMVector

-- | Create a new growable vector from an immutable vector.
--
-- This is an alias of 'thaw'.
fromVector :: (PrimMonad m) => V.Vector a -> m (GrowMVector (PrimState m) a)
fromVector = GG.fromVector

-- | Append an element to the vector, growing if necessary.
--
-- This takes \(O(1)\) amortized time. If there is sufficient
-- capacity, this takes \(O(1)\) time; otherwise, this operation takes
-- \(O(n)\) time.
append :: (PrimMonad m) => GrowMVector (PrimState m) a -> a -> m ()
append = GG.append

-- | Ensure the capacity of the vector is at least the given size.
--
-- If new space must be reserved, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
reserve :: (PrimMonad m) => GrowMVector (PrimState m) a -> Int -> m ()
reserve = GG.reserve

-- | Discard excess capacity.
--
-- If there is excess capacity, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
conserve :: (PrimMonad m) => GrowMVector (PrimState m) a -> m ()
conserve = GG.conserve

-- | Shrink the vector to the given size.
--
-- This is an \(O(n)\) operation, due to cleaning up discarded
-- references.
shrink :: (PrimMonad m) => GrowMVector (PrimState m) a -> Int -> m ()
shrink = GG.shrink

length :: (PrimMonad m) => GrowMVector (PrimState m) a -> m Int
length = GG.length

withMVector :: (PrimMonad m) => GrowMVector (PrimState m) a -> (MV.MVector (PrimState m) a -> m b) -> m b
withMVector = GG.withMVector

-- | Access the underlying mutable vector.
--
-- The vector returned is only valid until a call to `append`,
-- `reserve`, or `conserve`; after any of these calls, the
-- `GrowMVector` might be pointing to a new `MVector`.
unsafeWithMVector :: (PrimMonad m) => GrowMVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
unsafeWithMVector = GG.unsafeWithMVector

-- | The current capacity of the vector.
--
-- Up to this many elements may be stored in the vector without
-- requiring a resize operation.
capacity :: (PrimMonad m) => GrowMVector (PrimState m) a -> m Int
capacity = GG.capacity

read :: (PrimMonad m) => GrowMVector (PrimState m) a -> Int -> m a
read = GG.read

readMaybe :: (PrimMonad m) => GrowMVector (PrimState m) a -> Int -> m (Maybe a)
readMaybe = GG.readMaybe

write :: (PrimMonad m) => GrowMVector (PrimState m) a -> Int -> a -> m ()
write = GG.write

swap :: (PrimMonad m) => GrowMVector (PrimState m) a -> Int -> Int -> m ()
swap = GG.swap

type GenericConstructor gv s a = STRef s (GrowVectorState (GG.MVector gv) s a) -> gv s a

type GenericRef gv s a = gv s a -> STRef s (GrowVectorState (GG.MVector gv) s a)

genericGrowVectorEmpty :: (GG.GrowVector gv a) => (STRef s (GrowVectorState (GG.MVector gv) s a) -> gv s a) -> Int -> ST s (gv s a)
genericGrowVectorEmpty f initCapacity = do
  mv <- MG.unsafeNew initCapacity
  ref <- newSTRef (GrowVectorState mv 0 initCapacity)
  pure (f ref)

genericGrowVectorFromMVector :: (GG.GrowVector gv a) => GenericConstructor gv s a -> (GG.MVector gv) s a -> ST s (gv s a)
genericGrowVectorFromMVector f mv =
  fmap f (newSTRef (GrowVectorState mv (MG.length mv) (MG.length mv)))

genericGrowVectorAppend :: (GG.GrowVector gv a) => GenericRef gv s a -> gv s a -> a -> ST s ()
genericGrowVectorAppend getRef gv x =
  let ref = getRef gv
   in do
        (GrowVectorState _ initSize initCapacity) <- readSTRef ref
        when (initSize >= initCapacity) (genericGrowVectorReserve getRef gv (max 1 initSize * 2))
        (GrowVectorState v size _) <- readSTRef ref
        MG.write v size x
        let capacity'' = MG.length v
            size' = size + 1
        writeSTRef ref (GrowVectorState v size' capacity'')

genericGrowVectorReserve :: (GG.GrowVector gv a) => GenericRef gv s a -> gv s a -> Int -> ST s ()
genericGrowVectorReserve getRef gv newCapacity =
  let ref = getRef gv
   in do
        (GrowVectorState v size capacity') <- readSTRef ref
        when (capacity' < newCapacity) $ do
          v' <- MG.grow v (newCapacity - capacity')
          writeSTRef ref (GrowVectorState v' size newCapacity)

genericGrowVectorConserve :: (GG.GrowVector gv a) => GenericRef gv s a -> gv s a -> ST s ()
genericGrowVectorConserve getRef gv =
  let ref = getRef gv
   in do
        (GrowVectorState v size capacity') <- readSTRef ref
        when (size < capacity') $ do
          v' <- MG.unsafeNew size
          forM_ [0 .. size - 1] $ \i -> MG.read v i >>= MG.write v' i
          writeSTRef ref (GrowVectorState v' size size)

genericGrowVectorShrink :: (GG.GrowVector gv a) => GenericRef gv s a -> gv s a -> Int -> ST s ()
genericGrowVectorShrink getRef gv maxSize =
  let ref = getRef gv
   in do
        (GrowVectorState v size capacity') <- readSTRef ref
        when (size > maxSize) $ do
          MG.clear (MG.slice maxSize (capacity' - maxSize) v)
          writeSTRef ref (GrowVectorState v maxSize capacity')

genericGrowVectorMVector :: (GG.GrowVector gv a) => GenericRef gv s a -> gv s a -> ST s ((GG.MVector gv) s a)
genericGrowVectorMVector getRef gv =
  let ref = getRef gv
   in do
        (GrowVectorState v size _) <- readSTRef ref
        pure (MG.slice 0 size v)

genericGrowVectorCapacity :: GenericRef gv s a -> gv s a -> ST s Int
genericGrowVectorCapacity getRef gv =
  let ref = getRef gv
   in do
        (GrowVectorState _ _ capacity') <- readSTRef ref
        pure capacity'

genericGrowVectorLength :: GenericRef gv s a -> gv s a -> ST s Int
genericGrowVectorLength getRef gv =
  let ref = getRef gv
   in do
        (GrowVectorState _ length' _) <- readSTRef ref
        pure length'
