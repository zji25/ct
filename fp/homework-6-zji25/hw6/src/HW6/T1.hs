{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-|
Module      : HW6.T1
Description : thread-safe separately chained hash table
this module contains an implementation of a thread-safe separately chained 
hash table using STM

__example:__

@
> cht <- newCHT :: IO (CHT (STM IO) String Int)
> putCHT "key" 1 cht
> getCHT "key" cht
Just 1
@
-}

module HW6.T1
  ( Bucket
  , BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy 
  ( MonadConc (..)
  , STM
  , newTVar
  , readTVar
  , writeTVar
  , modifyTVar'
  )
import Control.Concurrent.Classy.STM (TArray, TVar, MonadSTM)
import Control.Monad (forM_, when)
import Data.Array.Base (getNumElements)
import Data.Array.MArray (MArray, getElems, newArray, readArray, writeArray)
import Data.Hashable (Hashable, hash)
import Data.Maybe (isNothing)
import GHC.Float.RealFracMethods (floorDoubleInt, int2Double)


-- | initial capacity of the hash table
initCapacity :: Int
initCapacity = 16

-- | load factor used for resizing the hash table to maintain the performance
-- 
-- @capacity =@ 'chtBuckets' ' length is doubled when
-- 'chtSize' @>= capacity * loadFactor@
loadFactor :: Double
loadFactor = 0.75

-- | type synonym for a bucket, represents a list of key-value pairs
type Bucket k v = [(k, v)]

-- | type synonym for an array of buckets in STM
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | the hash table datatype
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  -- ^ array of buckets
  , chtSize    :: TVar stm Int
  -- ^ number of elements in the hash table
  }

-- | \(O(1)\) creates a new empty hash table
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ CHT 
  <$> (newBucketsArray initCapacity >>= newTVar) 
  <*> newTVar 0

-- | \(O(1)\) retrieves a value associated with a specified key 
-- from the hash table
getCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT key (CHT buckets _) = atomically $ do
  (array, capacity) <- unwrapBuckets buckets
  lookup key <$> readArray array (index key capacity)

-- | amortized \(O(1)\) inserts a key-value pair into the hash table
-- 
-- if the key was present in the table, updates the associated value
putCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key val (CHT buckets size) = 
  atomically putPair >> atomically resizeIfNeeded
  where
  putPair = do
    (array, capacity) <- unwrapBuckets buckets
    let ind = index key capacity
    bucket <- readArray array ind
    writeArray array ind $ (key, val) : filter ((/= key) . fst) bucket
    writeTVar buckets array
    when (isNothing $ lookup key bucket) $ modifyTVar' size (+1)
  resizeIfNeeded = do
    (array, capacity) <- unwrapBuckets buckets
    readTVar size >>= \s ->
      when (s >= (floorDoubleInt $ loadFactor * int2Double capacity)) $ do
        let capacity' = capacity*2
        elems <- getElems array
        array' <- newBucketsArray capacity'
        forM_ elems $ flip forM_ $ \pair -> do
          let ind' = index (fst pair) capacity'
          (pair :) <$> readArray array' ind' >>= writeArray array' ind'
        writeTVar buckets array'

-- | \(O(1)\) retrieves the number of elements in the hash table
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT (CHT _ size) = readTVarConc size


index :: Hashable k => k -> Int -> Int
index key capacity = hash key `mod` capacity

newBucketsArray :: MArray a (Bucket k v) m => Int -> m (a Int (Bucket k v))
newBucketsArray size = newArray (0, size-1) []

unwrapBuckets 
  :: MonadSTM stm 
  => TVar stm (BucketsArray stm k v) 
  -> stm ((BucketsArray stm k v), Int)
unwrapBuckets buckets = do 
  array <- readTVar buckets
  capacity <- getNumElements array
  return (array, capacity)
