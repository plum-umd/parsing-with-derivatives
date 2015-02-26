{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, ImpredicativeTypes #-}

module DerParser.Parser where

import Text.Printf
import Data.Functor
import Data.List
import Control.Monad
import Data.IORef
import Data.Map (Map)
import DerParser.FPMaybe
import DerParser.ResultType
import qualified Data.Map as Map

initKnotValues :: IO (KnotValues a)
initKnotValues = do
  derive <- newIORef (Map.empty)
  fixMetaStep <- newIORef Nothing
  freezeMeta <- newIORef Nothing
  weed <- newIORef Nothing
  compact <- newIORef Nothing
  compactTop <- newIORef Nothing
  return KnotValues 
    { knotValuesDerive      = derive
    , knotValuesFixMetaStep = fixMetaStep
    , knotValuesFreezeMeta  = freezeMeta
    , knotValuesWeed        = weed
    , knotValuesCompact     = compact
    , knotValuesCompactTop  = compactTop
    }

data Parser a where
  Parser :: (ResultType a) =>
    { parserParserRec   :: ParserRec a
    , parserMeta        :: FPMaybe Meta
    , parserChanged     :: Bool
    , parserPhase       :: Phase0
    , parserKnotValues  :: KnotValues a
    } -> Parser a

data Phase0 = NoPhase { phase0 :: Phase1 } | Regressed { phase0 :: Phase1 } | Zipped { phase0 :: Phase1 }
data Phase0Type = NoPhaseType | RegressedType | ZippedType
  deriving (Eq, Ord, Show)

phase0Type :: Phase0 -> Phase0Type
phase0Type (NoPhase _)   = NoPhaseType
phase0Type (Regressed _) = RegressedType
phase0Type (Zipped _)    = ZippedType

data Phase1 = Dirty | Frozen | Weeded
  deriving (Eq, Ord, Show)

data EpsValue a where
  EpsValue  :: a -> EpsValue a
  EpsDValue :: EpsValue String

data ParserRec a where
  Con :: (ResultType a1, ResultType a2) => Parser a1 -> Parser a2 -> ParserRec (a1, a2)
  Alt :: (ResultType a)                 => Parser a -> Parser a -> ParserRec a
  Red :: (ResultType a, ResultType b)   => Parser a -> (a -> b) -> ParserRec b
  Ter ::                                   String -> ParserRec String
  Eps :: (ResultType a)                 => EpsValue a -> ParserRec a
  Emp :: (ResultType a)                 => ParserRec a
  Zip :: (ResultType a, ResultType b)   => Parser a -> ParserContext a b -> ParserRec b
  Nul :: (ResultType a)                 => Parser a -> ParserRec a

data ParserRecType = ConType | AltType | RedType | TerType | EpsType | EmpType | ZipType | NulType
  deriving (Eq, Ord, Show)

data ParserContext a b where
  ConRHole :: (ResultType a, ResultType b, ResultType c) => Parser a -> ParserContext (a,b) c -> ParserContext b c
  ConLHole :: (ResultType a, ResultType b, ResultType c) => Parser b -> ParserContext (a,b) c -> ParserContext a c
  RedHole  :: (ResultType a, ResultType b, ResultType c) => (a -> b) -> ParserContext b c -> ParserContext a c
  NulHole  :: (ResultType a, ResultType b)               => ParserContext a b -> ParserContext a b
  PCTop    :: (ResultType a)                             => ParserContext a a

data Meta = Meta
  { metaNullable  :: Bool
  , metaEmpty     :: Bool
  , metaFailAll   :: Bool
  } deriving (Eq, Show)

newtype KFun a = KFun (forall b. (ResultType a, ResultType b) => ParserContext a b -> IO (Parser b))

data KnotValues a = KnotValues
  { knotValuesDerive      :: IORef (Map String (Parser a))
  , knotValuesFixMetaStep :: IORef (Maybe (Parser a))
  , knotValuesFreezeMeta  :: IORef (Maybe (Parser a))
  , knotValuesWeed        :: IORef (Maybe (Parser a))
  , knotValuesCompact     :: IORef (Maybe (KFun a))
  , knotValuesCompactTop  :: IORef (Maybe (Parser a))
  }

knotValuesStatus :: KnotValues a -> IO String
knotValuesStatus (KnotValues _ fmsP fmP wP cP ctP) = do
  fms <- readIORef fmsP
  fm  <- readIORef fmP
  w   <- readIORef wP
  c   <- readIORef cP
  ct  <- readIORef ctP
  return $ printf "%s%s%s%s%s" (maybe "" (const "M") fms)
                               (maybe "" (const "F") fm)
                               (maybe "" (const "W") w)
                               (maybe "" (const "C") c)
                               (maybe "" (const "T") ct)

parserType :: Parser a -> ParserRecType
parserType = parserRecType . parserParserRec
  where
    parserRecType (Con _ _) = ConType
    parserRecType (Alt _ _) = AltType
    parserRecType (Red _ _) = RedType
    parserRecType (Ter _)   = TerType
    parserRecType (Eps _)   = EpsType
    parserRecType Emp       = EmpType
    parserRecType (Zip _ _) = ZipType
    parserRecType (Nul _)   = NulType

newtype GenParser = GenParser { unGenParser :: forall c. (forall b. Parser b -> c) -> c }

genParser :: Parser a -> GenParser
genParser p = GenParser $ \ f -> f p

parserChildren :: Parser a -> IO [GenParser]
parserChildren p = do
  cs <- parserDeriveChildren p
  return $ parserImmediateChildren p ++ map genParser cs

parserImmediateChildren :: Parser a -> [GenParser]
parserImmediateChildren = parserRecImmediateChildren . parserParserRec
  where
    parserRecImmediateChildren (Con a b) = [genParser a, genParser b]
    parserRecImmediateChildren (Alt a b) = [genParser a, genParser b]
    parserRecImmediateChildren (Red a _) = [genParser a]
    parserRecImmediateChildren (Ter _)   = []
    parserRecImmediateChildren (Eps _)   = []
    parserRecImmediateChildren Emp       = []
    parserRecImmediateChildren (Zip a _) = [genParser a]
    parserRecImmediateChildren (Nul a)   = [genParser a]

parserDeriveChildren :: Parser a -> IO [Parser a]
parserDeriveChildren = liftM Map.elems . readIORef . parserKnotDerive

mapParserChildren :: (forall b. Parser b -> Parser b) -> Parser a -> IO (Parser a)
mapParserChildren f = (mapParserDeriveChildren f =<<) .  mapParserImmediateChildren f

mapParserImmediateChildren :: (forall b. Parser b -> Parser b) -> Parser a -> IO (Parser a)
mapParserImmediateChildren f (Parser r m c s k) = Parser (mapParserRecImmediateChildren r) m c s <$> (keepDerive k =<< initKnotValues)
  where
    mapParserRecImmediateChildren :: ParserRec a -> ParserRec a
    mapParserRecImmediateChildren (Con a b) = Con (f a) (f b)
    mapParserRecImmediateChildren (Alt a b) = Alt (f a) (f b)
    mapParserRecImmediateChildren (Red a g) = Red (f a) g
    mapParserRecImmediateChildren p@(Ter _) = p
    mapParserRecImmediateChildren p@(Eps _) = p
    mapParserRecImmediateChildren p@Emp     = p
    mapParserRecImmediateChildren (Zip a p) = Zip (f a) p
    mapParserRecImmediateChildren (Nul a)   = Nul (f a)

keepDerive :: KnotValues a -> KnotValues a -> IO (KnotValues a)
keepDerive k r = do
  v <- readIORef $ knotValuesDerive k
  writeIORef (knotValuesDerive r) v
  return r

setParserKnotValues :: KnotValues a -> Parser a -> Parser a
setParserKnotValues k p = p { parserKnotValues = k }

mapParserDeriveChildren :: (Parser a -> Parser a) -> Parser a -> IO (Parser a)
mapParserDeriveChildren f p = do
  cs <- readIORef $ parserKnotDerive p
  newRef <- newIORef $ Map.map f cs
  return p { parserKnotValues = (parserKnotValues p) { knotValuesDerive = newRef } }

foldlParserChildren :: (forall b. t -> Parser b -> t) -> t -> Parser a -> IO t
foldlParserChildren f i p = foldlParserDeriveChildren f (foldlParserImmediateChildren f i p) p

foldlParserImmediateChildren :: (forall b. t -> Parser b -> t) -> t -> Parser a -> t
foldlParserImmediateChildren f i p = foldl' g i $ parserImmediateChildren p
  where
    g t (GenParser h) = h (f t)

foldlParserDeriveChildren :: (forall b. t -> Parser b -> t) -> t -> Parser a -> IO t
foldlParserDeriveChildren f i p = liftM (foldl' f i) $ parserDeriveChildren p

foldlParserChildrenM :: (forall b. t -> Parser b -> IO t) -> t -> Parser a -> IO t
foldlParserChildrenM f i p = do
  i' <- foldlParserImmediateChildrenM f i p
  foldlParserDeriveChildrenM f i' p

foldlParserImmediateChildrenM :: (forall b. t -> Parser b -> IO t) -> t -> Parser a -> IO t
foldlParserImmediateChildrenM f i p = foldM g i $ parserImmediateChildren p
  where
    g t (GenParser h) = h (f t)

foldlParserDeriveChildrenM :: (forall b. t -> Parser b -> IO t) -> t -> Parser a -> IO t
foldlParserDeriveChildrenM f i p = do
  cs <- parserDeriveChildren p
  foldM f i cs

parserKnotDerive :: Parser a -> IORef (Map String (Parser a))
parserKnotDerive = knotValuesDerive . parserKnotValues

parserKnotFixMetaStep :: Parser a -> IORef (Maybe (Parser a))
parserKnotFixMetaStep = knotValuesFixMetaStep . parserKnotValues

parserKnotFreezeMeta :: Parser a -> IORef (Maybe (Parser a))
parserKnotFreezeMeta = knotValuesFreezeMeta . parserKnotValues

parserKnotWeed :: Parser a -> IORef (Maybe (Parser a))
parserKnotWeed = knotValuesWeed . parserKnotValues

parserKnotCompact :: Parser a -> IORef (Maybe (KFun a))
parserKnotCompact = knotValuesCompact . parserKnotValues

parserKnotCompactTop :: Parser a -> IORef (Maybe (Parser a))
parserKnotCompactTop = knotValuesCompactTop . parserKnotValues

nullable :: Parser a -> Bool
nullable = metaNullable . fpMaybe . parserMeta

empty :: Parser a -> Bool
empty = metaEmpty . fpMaybe . parserMeta

failAll :: Parser a -> Bool
failAll = metaFailAll . fpMaybe . parserMeta

decided :: Parser a -> Bool
decided = isDecided . parserMeta

cached :: (Parser a -> IORef (t v))
       -> (t v -> Maybe v) 
       -> (v -> t v -> t v) 
       -> (Parser a -> IO v)
       -> Parser a
       -> IO v
cached cell lookupVal insertVal compute p = do
  valM <- liftM lookupVal . readIORef $ cell p
  let computeAndCache = do
        result <- compute p
        modifyIORef (cell p) (insertVal result)
        return result
  maybe computeAndCache return valM
