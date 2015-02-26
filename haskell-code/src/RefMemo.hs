{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module RefMemo
  ( weakMemoGen
  ) where

import Control.Monad
import Data.HashTable (HashTable)
import Data.IORef
import Data.Map (Map)
import DynamicStableName
import System.IO.Unsafe
import System.Mem.Weak
import qualified Data.Map as Map

weakMemoGen :: forall c k a b. Container IO c k -> (a -> IO k) -> (a -> b) -> (a -> b)
weakMemoGen c cKey f = unsafePerformIO $ do
  -- make a new table
  table <- containerNew c :: IO (c k (Weak v))
  -- we need a week pointer to the table so that it can be collected if nobody
  -- else requires it
  tableWeak <- mkWeakPtr table (Just (tableFinalizer c table))
  return $ \ a -> unsafePerformIO $ do
    -- name the next result (will only be used if we don't find it in the
    -- cache)
    let b = f a
    -- compute the key
    akey <- cKey a
    -- see if there is an entry in the table
    bWeakM <- containerLookup c akey table
    case bWeakM of
      -- if it is not in the table, add it
      Nothing -> addToTable c a akey b table tableWeak
      (Just bWeak) -> do
        -- if it is in the table, see if the week pointer is still connected to
        -- the value
        bM <- deRefWeak bWeak
        case bM of
          -- if it is not, we must add the new value (should this ever happen?)
          Nothing -> addToTable c a akey b table tableWeak
          -- if it is, we have found it
          (Just b) -> return b
  where
    -- how to finalize the table if it is no longer needed:
    -- we must finalize all of the elements
    tableFinalizer c table = do
      es <- containerValues c table
      forM_ es finalize
    -- if the value is collected, remove the value from the table
    -- how to add this value to the table
    valFinalizer c akey tableWeak = do
      tableM <- deRefWeak tableWeak
      case tableM of
        Nothing -> return ()
        (Just table) -> containerDelete c akey table
    -- add to the cache table, and set up a finalizer
    addToTable c a akey b table tableWeak = do
      bWeak <- mkWeak a b (Just (valFinalizer c akey tableWeak))
      containerInsert c akey bWeak table
      return b
    

-- strongMemoGen :: forall c k a b.
--                  (forall v. IO (c k v))
--               -> (forall v. k -> c k v -> IO (Maybe v))
--               -> (forall v. k -> v -> c k v -> IO ())
--               -> (a -> IO k)
--               -> (a -> b)
--               -> (a -> b)
-- strongMemoGen htnew htlookup htinsert htkey f = unsafePerformIO $ do
--   table <- htnew
--   return $ \ a -> unsafePerformIO $ do
--     akey <- htkey a
--     let computedVal = f a
--         addToTable = do
--           htinsert akey computedVal table
--           return computedVal
--     valM <- htlookup akey table
--     case valM of
--       Nothing -> addToTable
--       (Just val) -> return val

-- strongRefMemo :: forall a b. (a -> b) -> (a -> b)
-- strongRefMemo = strongMemoGen htnew htlookup htinsert htkey
--   where
--     htnew :: forall v. IO (HashTable DynamicStableName v)
--     htnew = HT.new (==) (fromIntegral . hashStableName . unDynamicStableName)
--     htlookup :: forall v. DynamicStableName -> HashTable DynamicStableName v -> IO (Maybe v)
--     htlookup k t = HT.lookup t k
--     htinsert :: forall v. DynamicStableName -> v -> HashTable DynamicStableName v -> IO ()
--     htinsert k v t = HT.insert t k v
--     htkey a
--       | a `seq` True = makeDynStableName a
--       | otherwise = error "seq failed"

-- newtype IORefMap k v = IORefMap { unIORefMap :: IORef (Map k v) }
-- 
-- weakEqMemo :: forall a b. (Ord a) => (a -> b) -> (a -> b)
-- weakEqMemo = weakMemoGen htnew htlookup htinsert htdelete htelems htkey
--   where
--     htnew :: forall v. IO (IORefMap a v)
--     htnew = liftM IORefMap $ newIORef Map.empty
--     htlookup :: forall v. a -> IORefMap a v -> IO (Maybe v)
--     htlookup k = liftM (Map.lookup k) . readIORef . unIORefMap
--     htinsert :: forall v. a -> v -> IORefMap a v -> IO ()
--     htinsert k v = flip modifyIORef (Map.insert k v) . unIORefMap
--     htdelete :: forall v. a -> IORefMap a v -> IO ()
--     htdelete k = flip modifyIORef (Map.delete k) . unIORefMap
--     htelems :: forall v. IORefMap a v -> IO [v]
--     htelems = liftM Map.elems . readIORef . unIORefMap
--     htkey :: a -> IO a
--     htkey = return
-- 
-- strongEqMemo :: forall a b. (Ord a) => (a -> b) -> (a -> b)
-- strongEqMemo = strongMemoGen htnew htlookup htinsert htkey
--   where
--     htnew :: forall v. IO (IORefMap a v)
--     htnew = liftM IORefMap $ newIORef Map.empty
--     htlookup :: forall v. a -> IORefMap a v -> IO (Maybe v)
--     htlookup k = liftM (Map.lookup k) . readIORef . unIORefMap
--     htinsert :: forall v. a -> v -> IORefMap a v -> IO ()
--     htinsert k v = flip modifyIORef (Map.insert k v) . unIORefMap
--     htkey :: a -> IO a
--     htkey = return

