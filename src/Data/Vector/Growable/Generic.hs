{-# LANGUAGE TypeOperators #-}

module Data.Vector.Growable.Generic
  ( -- * Growable vectors
    GrowVector (..),

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
import Control.Monad.ST
import Data.Kind
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import Prelude hiding (length, read)

-- | A vector that can grow (and shrink).
--
-- Although a `GrowVector` is not a `Data.Vector.Generic.Mutable`
-- instance, you can get at one via the `modifying` or
-- `unsafeMVector` methods. The reason for this indirection is the
-- `Data.Vector.Generic.Mutable.length` method, which requires the
-- length of the vector to be known purely. While it would be possible
-- to store the length of a growable vector in a way so as to satisfy
-- this API, it would make it easy for stale references to contain
-- garbage length values. In the following example, imagine we have
-- declared `GrowVector` to implement
-- `Data.Vector.Mutable.Generic.MVector`.
--
-- @
--     example = do
--       gv <- fromVector (V.fromList [1..100])
--       shrink gv 10
--       print (MV.length gv)
-- @
--
-- What should be printed? Semantically, we want "10" to be printed;
-- however, since `MV.length gv` cannot invoke side effects, it will
-- likely print 100 instead. Fixing this would likely require us to
-- return new references as we go, as in
--
-- @
--     example = do
--       gv <- fromVector (V.fromList [1..100])
--       gv' <- shrink gv 10
--       print (MV.length gv')
-- @
--
-- but this requires a lot of manual bookkeeping, and is still error-prone.
class (MG.MVector (MVector v) a) => GrowVector v a where
  type MVector v :: Type -> Type -> Type

  basicEmpty :: Int -> ST s (v s a)
  basicFromMVector :: MVector v s a -> ST s (v s a)
  basicAppend :: v s a -> a -> ST s ()
  basicReserve :: v s a -> Int -> ST s ()
  basicConserve :: v s a -> ST s ()
  basicShrink :: v s a -> Int -> ST s ()
  basicMVector :: v s a -> ST s (MVector v s a)
  basicLength :: v s a -> ST s Int
  basicCapacity :: v s a -> ST s Int

  basicWrite :: v s a -> Int -> a -> ST s ()
  basicWrite gv i x = withMVector gv (\mv -> MG.write mv i x)

  basicRead :: v s a -> Int -> ST s a
  basicRead gv i = withMVector gv (`MG.read` i)

  basicReadMaybe :: v s a -> Int -> ST s (Maybe a)
  basicReadMaybe gv i = withMVector gv (`MG.readMaybe` i)

-- | Create an empty growable vector with the given initial capacity.
empty :: (PrimMonad m, GrowVector v a) => Int -> m (v (PrimState m) a)
empty = stToPrim . basicEmpty

-- | Create an immutable copy of the grow vector.
freeze :: (PrimMonad m, s ~ PrimState m, G.Vector v a, MVector gv ~ G.Mutable v, GrowVector gv a) => gv s a -> m (v a)
freeze = stToPrim . flip withMVector G.freeze

-- | Create a growable copy of the vector.
--
-- Assumed complexity \(O(n)\).
thaw :: (PrimMonad m, s ~ PrimState m, G.Vector v a, MVector gv ~ G.Mutable v, GrowVector gv a) => v a -> m (gv s a)
thaw = stToPrim . (G.thaw >=> basicFromMVector)

-- | Create a new growable vector from an existing mutable vector.
--
-- The mutable vector should be considered unsafe to use after calling
-- this; see `fromVector` for a safer alternative.
--
-- Assumed complexity \(O(1)\).
fromMVector :: (PrimMonad m, GrowVector v a) => MVector v (PrimState m) a -> m (v (PrimState m) a)
fromMVector = stToPrim . basicFromMVector

-- | Create a new growable vector from an existing vector.
--
-- This is an alias of 'thaw'.
fromVector ::
  (MVector gv ~ G.Mutable v, PrimMonad m, G.Vector v a, GrowVector gv a) =>
  v a ->
  m (gv (PrimState m) a)
fromVector = thaw

-- | Append an element to the vector, growing if necessary.
--
-- Assumed complexity amortized \(O(1)\).
append :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> a -> m ()
append v = stToPrim . basicAppend v

-- | Ensure the capacity of the vector is at least the given size.
--
-- Assumed complexity \(O(n)\) when resizing is necessary.
reserve :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> Int -> m ()
reserve v = stToPrim . basicReserve v

-- | Discard excess capacity.
--
-- Assumed time \(O(n)\) when resizing is necessary.
conserve :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> m ()
conserve = stToPrim . basicConserve

shrink :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> Int -> m ()
shrink v = stToPrim . basicShrink v

-- | Manipulate the underlying mutable vector.
--
-- The underlying vector should not be exfiltrated from this function,
-- since this value is only safe to use until `append`, `reserve`, or
-- `conserve` are called.
withMVector :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> (MVector v (PrimState m) a -> m b) -> m b
withMVector gv f = do
  len <- length gv
  mv <- unsafeWithMVector gv
  f (MG.slice 0 len mv)

-- | Manipulate the entire underlying mutable vector, including allocated
-- but unused space.
--
-- It is likely this value is only safe to use until `append`,
-- `reserve`, or `conserve` are called.
--
-- Assumed complexity \(O(1)\).
unsafeWithMVector :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> m (MVector v (PrimState m) a)
unsafeWithMVector = stToPrim . basicMVector

-- | The current length of the vector.
--
-- Assumed complexity \(O(1)\).
length :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> m Int
length = stToPrim . basicLength

-- | The current capacity of the vector.
--
-- Up to this many elements may be stored in the vector without
-- requiring a resize operation.
--
-- Assumed complexity \(O(1)\).
capacity :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> m Int
capacity = stToPrim . basicCapacity

write :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> Int -> a -> m ()
write v i = stToPrim . basicWrite v i

read :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> Int -> m a
read v = stToPrim . basicRead v

readMaybe :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> Int -> m (Maybe a)
readMaybe v = stToPrim . basicReadMaybe v

swap :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> Int -> Int -> m ()
swap gv i j = do
  xi <- read gv i
  xj <- read gv j
  write gv i xj
  write gv j xi
