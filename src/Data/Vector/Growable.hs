{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Growable
  ( -- * Growable vectors
    GrowVector,

    -- ** Type aliases
    GrowVectorIO,
    GrowVectorST,

    -- ** Construction
    new,
    fromMVector,
    fromVector,

    -- ** Growing
    append,
    reserve,

    -- ** Shrinking
    conserve,
    shrink,

    -- ** Information
    capacity,

    -- ** Manipulation
    mvector,
  )
where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Growable.Generic as GG

type GrowVectorIO v a = GrowVector v (PrimState IO) a

type GrowVectorST v s a = GrowVector v (PrimState (ST s)) a

-- | A vector that can grow (and shrink).
--
-- A `GrowVector` is associated with an `Data.Vector.Generic.Mutable`
-- instance via an associated type.
newtype GrowVector v s a = GrowVector
  { _growVectorState :: STRef s (GrowVectorState v s a)
  }

data GrowVectorState v s a = GrowVectorState
  { _growVectorStateVector :: v s a,
    _growVectorStateSize :: Int,
    _growVectorStateCapacity :: Int
  }

instance MG.MVector v a => GG.GrowVector (GrowVector v) a where
  type MVector (GrowVector v) = v

  basicNew = growVectorNew
  basicFromMVector = growVectorFromMVector
  basicAppend = growVectorAppend
  basicReserve = growVectorReserve
  basicConserve = growVectorConserve
  basicShrink = growVectorShrink
  basicMVector = growVectorMVector
  basicLength = growVectorLength
  basicCapacity = growVectorCapacity

growVectorNew :: MG.MVector v a => Int -> ST s (GrowVector v s a)
growVectorNew initCapacity = do
  mv <- MG.unsafeNew initCapacity
  ref <- newSTRef (GrowVectorState mv 0 initCapacity)
  pure (GrowVector ref)

growVectorFromMVector :: MG.MVector v a => v s a -> ST s (GrowVector v s a)
growVectorFromMVector mv =
  fmap GrowVector (newSTRef (GrowVectorState mv (MG.length mv) (MG.length mv)))

growVectorAppend :: MG.MVector v a => GrowVector v s a -> a -> ST s ()
growVectorAppend gv@(GrowVector ref) x = do
  (GrowVectorState _ initSize initCapacity) <- readSTRef ref
  when (initSize >= initCapacity) (growVectorReserve gv (max 1 initSize * 2))
  (GrowVectorState v size _) <- readSTRef ref
  MG.write v size x
  let capacity'' = MG.length v
      size' = size + 1
  writeSTRef ref (GrowVectorState v size' capacity'')

growVectorReserve :: MG.MVector v a => GrowVector v s a -> Int -> ST s ()
growVectorReserve (GrowVector ref) newCapacity = do
  (GrowVectorState v size capacity') <- readSTRef ref
  when (capacity' < newCapacity) $ do
    v' <- MG.grow v (newCapacity - capacity')
    writeSTRef ref (GrowVectorState v' size newCapacity)

growVectorConserve :: MG.MVector v a => GrowVector v s a -> ST s ()
growVectorConserve (GrowVector ref) = do
  (GrowVectorState v size capacity') <- readSTRef ref
  when (size < capacity') $ do
    v' <- MG.unsafeNew size
    forM_ [0 .. size - 1] $ \i -> MG.read v i >>= MG.write v' i
    writeSTRef ref (GrowVectorState v' size size)

growVectorShrink :: MG.MVector v a => GrowVector v s a -> Int -> ST s ()
growVectorShrink (GrowVector ref) maxSize = do
  (GrowVectorState v size capacity') <- readSTRef ref
  when (size > maxSize) $ do
    MG.clear (MG.slice maxSize (capacity' - maxSize) v)
    writeSTRef ref (GrowVectorState v maxSize capacity')

growVectorMVector :: MG.MVector v a => GrowVector v s a -> ST s (v s a)
growVectorMVector (GrowVector ref) = do
  (GrowVectorState v size _) <- readSTRef ref
  pure (MG.slice 0 size v)

growVectorCapacity :: GrowVector v s a -> ST s Int
growVectorCapacity (GrowVector ref) = do
  (GrowVectorState _ _ capacity') <- readSTRef ref
  pure capacity'

growVectorLength :: GrowVector v s a -> ST s Int
growVectorLength (GrowVector ref) = do
  (GrowVectorState _ length' _ ) <- readSTRef ref
  pure length'

-- | Create a new growable vector
new :: (PrimMonad m, MG.MVector v a) => Int -> m (GrowVector v (PrimState m) a)
new = GG.new

-- | Create a new growable vector from an immutable vector.
fromMVector :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> m (GrowVector v (PrimState m) a)
fromMVector = GG.fromMVector

-- | Create a new growable vector from an immutable vector.
fromVector :: (PrimMonad m, G.Vector v a) => v a -> m (GrowVector (G.Mutable v) (PrimState m) a)
fromVector = GG.fromVector

-- | Append an element to the vector, growing if necessary.
--
-- This takes \(O(1)\) amortized time. If there is sufficient
-- capacity, this takes \(O(1)\) time; otherwise, this operation takes
-- \(O(n)\) time.
append :: (PrimMonad m, MG.MVector v a) => GrowVector v (PrimState m) a -> a -> m ()
append = GG.append

-- | Ensure the capacity of the vector is at least the given size.
--
-- If new space must be reserved, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
reserve :: (PrimMonad m, MG.MVector v a) => GrowVector v (PrimState m) a -> Int -> m ()
reserve = GG.reserve

-- | Discard excess capacity.
--
-- If there is excess capacity, this is an \(O(n)\) operation;
-- otherwise, it is \(O(1)\).
conserve :: (PrimMonad m, MG.MVector v a) => GrowVector v (PrimState m) a -> m ()
conserve = GG.conserve

-- | Shrink the vector to the given size.
--
-- This is an \(O(n)\) operation, due to cleaning up discarded
-- references.
shrink :: (PrimMonad m, MG.MVector v a) => GrowVector v (PrimState m) a -> Int -> m ()
shrink = GG.shrink

-- | Access the underlying mutable vector.
--
-- The vector returned is only valid until a call to `append`,
-- `reserve`, or `conserve`; after any of these calls, the
-- `GrowVector` might be pointing to a new `MVector`.
mvector :: (PrimMonad m, MG.MVector v a) => GrowVector v (PrimState m) a -> m (v (PrimState m) a)
mvector = GG.mvector

-- | The current capacity of the vector.
--
-- Up to this many elements may be stored in the vector without
-- requiring a resize operation.
capacity :: (PrimMonad m, MG.MVector v a) => GrowVector v (PrimState m) a -> m Int
capacity = GG.capacity
