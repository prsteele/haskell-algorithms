{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vector.Growable.Generic where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Kind
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Generic.New as GN

-- | A vector that can grow (and shrink).
--
-- Although a `GrowVector` is not a `Data.Vector.Generic.Mutable`
-- instance, you can get at one via the `mvector` method. The reason
-- for this indirection is the `Data.Vector.Generic.Mutable.length`
-- method, which requires the length of the vector to be known purely.
-- While it would be possible to store the length of a growable vector
-- in a way so as to satisfy this API, it would make it easy for stale
-- references to contain garbage length values. In the following
-- example, imagine we have declared `GrowVector` to implement
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

  basicNew :: Int -> ST s (v s a)
  basicFromMVector :: MVector v s a -> ST s (v s a)
  basicAppend :: v s a -> a -> ST s ()
  basicReserve :: v s a -> Int -> ST s ()
  basicConserve :: v s a -> ST s ()
  basicShrink :: v s a -> Int -> ST s ()
  basicMvector :: v s a -> ST s (MVector v s a)
  basicCapacity :: v s a -> ST s Int

-- | Create a new growable vector
new :: (PrimMonad m, GrowVector v a) => Int -> m (v (PrimState m) a)
new = stToPrim . basicNew

-- | Create a new growable vector from an existing mutable vector.
--
-- The mutable vector should be considered unsafe to use after calling
-- this; see `fromVector` for a safer alternative.
--
-- Assumed complexity \(O(1)\).
fromMVector :: (PrimMonad m, GrowVector v a) => MVector v (PrimState m) a -> m (v (PrimState m) a)
fromMVector = stToPrim . basicFromMVector

-- | Create a new growabe vector from an existing vector.
--
-- Assumed complexity \(O(n)\).
fromVector ::
  (MVector gv ~ G.Mutable v, PrimMonad m, G.Vector v a, GrowVector gv a) =>
  v a ->
  m (gv (PrimState m) a)
fromVector v = GN.runPrim (G.clone v) >>= fromMVector

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

-- | Access the underlying mutable vector.
--
-- It is likely this value is only safe to use until `append`,
-- `reserve`, or `conserve` are called.
--
-- Assumed complexity \(O(1)\).
mvector :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> m (MVector v (PrimState m) a)
mvector = stToPrim . basicMvector

-- | The current capacity of the vector.
--
-- Up to this many elements may be stored in the vector without
-- requiring a resize operation.
--
-- Assumed complexity \(O(1)\).
capacity :: (PrimMonad m, GrowVector v a) => v (PrimState m) a -> m Int
capacity = stToPrim . basicCapacity
