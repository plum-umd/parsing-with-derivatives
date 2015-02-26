{-# LANGUAGE RankNTypes #-}

module Container where

import Control.Monad
import Data.HashTable (HashTable)
import DynamicStableName
import System.Mem.StableName
import qualified Data.HashTable as HT

data Container m container key = Container
  { containerNew :: forall v. m (container key v)
  , containerLookup :: forall v. key -> container key v -> m (Maybe v)
  , containerInsert :: forall v. key -> v -> container key v -> m ()
  , containerModify :: forall v. key -> (Maybe v -> Maybe v) -> container key v -> m ()
  , containerModifyM :: forall v. key -> (Maybe v -> m (Maybe v)) -> container key v -> m ()
  , containerDelete :: forall v. key -> container key v -> m ()
  , containerValues :: forall v. container key v -> m [v]
  }

refContainer :: Container IO HashTable DynamicStableName 
refContainer = Container cNew cLookup cInsert cModify cModifyM cDelete cValues
  where
    cNew :: forall v. IO (HashTable DynamicStableName v)
    cNew = HT.new (==) (fromIntegral . hashStableName . unDynamicStableName)
    cLookup :: forall v. DynamicStableName -> HashTable DynamicStableName v -> IO (Maybe v)
    cLookup k t = HT.lookup t k
    cInsert :: forall v. DynamicStableName -> v -> HashTable DynamicStableName v -> IO ()
    cInsert k v t = HT.insert t k v
    cModify :: forall v. DynamicStableName -> (Maybe v -> Maybe v) -> HashTable DynamicStableName v -> IO ()
    cModify k f t = do
      v <- HT.lookup t k
      case f v of
        Nothing -> return ()
        (Just v') -> HT.insert t k v'
    cModifyM :: forall v. DynamicStableName -> (Maybe v -> IO (Maybe v)) -> HashTable DynamicStableName v -> IO ()
    cModifyM k f t = do
      v <- HT.lookup t k
      vM' <- f v
      case vM' of
        Nothing -> return ()
        (Just v') -> HT.insert t k v'
    cDelete :: forall v. DynamicStableName -> HashTable DynamicStableName v -> IO ()
    cDelete k t = HT.delete t k
    cValues :: forall v. HashTable DynamicStableName v -> IO [v]
    cValues = liftM (map snd) . HT.toList

refKey :: a -> IO DynamicStableName
refKey a
  | a `seq` True = makeDynStableName a
  | otherwise = error "seq failed"

