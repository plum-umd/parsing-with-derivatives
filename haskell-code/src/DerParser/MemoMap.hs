module DerParser.MemoMap where

import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as Map

memoFun :: (Ord a) => (a -> b) -> a -> b
memoFun f = unsafePerformIO $ do
  mapRef <- newIORef Map.empty
  return $ \a -> unsafePerformIO $ do
  currMap <- readIORef mapRef
  let vM = Map.lookup a currMap
  case vM of
    Just b -> return b
    Nothing -> do
      let b = f a
      writeIORef mapRef $ Map.insert a b currMap
      return b
