{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module DerParser.Reify where

import Control.Monad
import Data.IORef
import Data.List
import Data.Map (Map)
import DerParser.FPMaybe
import DerParser.Parser
import System.IO.Unsafe
import System.Mem.StableName
import Text.Printf
import Unsafe.Coerce
import qualified Data.Map as Map

type ParserInspect t =  (forall a. Parser a -> IO Integer) 
                     -> (forall a. Parser a -> IO Bool) 
                     -> (forall a. Parser a -> IO t)

inspectParser :: ParserInspect t -> Parser a -> t
inspectParser f p = unsafePerformIO $ do
  reifiedPt <- newIORef Map.empty
  seenPt    <- newIORef Map.empty
  uidPt     <- newIORef 1
  f (lookupId reifiedPt uidPt) (seenId seenPt) p

lookupId :: IORef (Map Int [(StableName (), Integer)]) 
         -> IORef Integer 
         -> Parser a 
         -> IO Integer
lookupId reifiedPt uidPt p | p `seq` True = do
  stblName <- genericStableName p
  let stblNameHashed = hashStableName stblName
  lookupValM <- liftM (extraLookup stblNameHashed stblName) $ readIORef reifiedPt
  case lookupValM of
    (Just lookupVal) -> return lookupVal
    Nothing          -> do
      thisId <- readIORef uidPt
      modifyIORef uidPt (+ 1)
      modifyIORef reifiedPt $ Map.insertWith (++) stblNameHashed [(stblName, thisId)]
      return thisId
lookupId _ _ _ | otherwise = error "seq failed"

seenId :: IORef (Map Int [(StableName (), ())]) -> Parser a -> IO Bool
seenId seenPt p | p `seq` True = do 
  stblName <- genericStableName p
  let stblNameHashed = hashStableName stblName
  lookupValM <- liftM (extraLookup stblNameHashed stblName) $ readIORef seenPt
  case lookupValM of
    (Just ()) -> return True
    Nothing -> do
      modifyIORef seenPt $ Map.insertWith (++) stblNameHashed [(stblName, ())]
      return False
seenId _ _ | otherwise = error "seq failed"

genericStableName :: a -> IO (StableName ())
genericStableName = liftM unsafeCoerce . makeStableName

extraLookup :: Int -> StableName () -> Map Int [(StableName (), a)] -> Maybe a
extraLookup hashed key m = process $ Map.lookup hashed m
  where
    process x = case x of
      (Just [])                                 -> Nothing
      (Just ((key', reified):xs)) | key == key' -> Just reified
                                  | otherwise   -> process (Just xs)
      Nothing                                   -> Nothing

type ParserFoldL t = forall a. t -> Parser a -> Integer -> Integer -> [Integer] -> [Integer] -> t

parserDeepFoldL :: ParserFoldL t -> t -> Parser a -> t
parserDeepFoldL f i p = inspectParser (inspectf f i) p

inspectf :: ParserFoldL t -> t -> ParserInspect t
inspectf f i uidM isSeenM p = do
  isSeen <- isSeenM p
  if isSeen then return i else do
    uid <- uidM p
    let getCuid (GenParser h) = h uidM
    cuids <- sequence . map getCuid $ parserImmediateChildren p
    dcuids <- sequence . map uidM =<< parserDeriveChildren p
    let pid = hashStableName (unsafePerformIO (genericStableName p))
    let next = f i p uid (fromIntegral pid) cuids dcuids
    foldlParserChildrenM (\t p' -> inspectf f t uidM isSeenM p') next p

data ParserInfo = ParserInfo
  { parserInfoUid                :: Integer
  , parserInfoPid                :: Integer
  , parserInfoType               :: ParserRecType
  , parserInfoMeta               :: FPMaybe Meta
  , parserInfoChanged            :: Bool
  , parserInfoStatus             :: Phase0
  , parserInfoChildrenUids       :: [Integer]
  , parserInfoDeriveChildrenUids :: [Integer]
  , parserInfoKnotValuesStatus   :: String
  }

parserToGraph :: Parser a -> [ParserInfo]
parserToGraph = reverse . parserDeepFoldL f []
  where
    f :: ParserFoldL [ParserInfo]
    f others p uid pid childrenids dchildrenids = ParserInfo uid
                                                             pid
                                                             (parserType p)
                                                             (parserMeta p)
                                                             (parserChanged p)
                                                             (parserPhase p)
                                                             childrenids
                                                             dchildrenids
                                                             (unsafePerformIO . knotValuesStatus $ parserKnotValues p) 
                                                 : others

showParserGraph :: [ParserInfo] -> String
showParserGraph = intercalate "\n" . map showParserGraphSingle
  where
    showParserGraphSingle :: ParserInfo -> String
    showParserGraphSingle (ParserInfo uid pid ptype meta changed status children dchildren kstatus) = 
      printf "%-6s%-6s%-6s%-10s%-10s%-10s%s %s changed:%s" 
             (show uid)
             (show pid)
             kstatus
             (show ptype)
             (show children)
             (show dchildren)
             (showMetaStuff meta)
             (showStatus status)
             (show changed)
    showMetaStuff :: FPMaybe Meta -> String
    showMetaStuff m = printf "{ %-13s :: nullable:%-5s empty:%-5s failAll:%-5s }" 
                             (show $ fpMaybeType m)
                             (show . metaNullable $ fpMaybe m)
                             (show . metaEmpty $ fpMaybe m)
                             (show . metaFailAll $ fpMaybe m)
    showStatus :: Phase0 -> String
    showStatus p = printf "phase0:%-11s phase1:%-6s"
                          (show $ phase0Type p)
                          (show $ phase0 p)

instance Show (Parser a) where
  show = showParserGraph . parserToGraph
