{-# LANGUAGE GADTs, RankNTypes #-}

module PWD where

import Control.Monad
import Data.HashTable (HashTable)
import Data.IORef
import Data.List
import Data.Set (Set)
import DynamicStableName
import RefMemo
import System.IO.Unsafe
import System.Mem.StableName
import Text.Printf
import qualified Data.HashTable as HT
import qualified Data.Set as Set

-- Parser Types

data Parser t a where
  Parser :: (Ord t, Ord a) =>
    { parserRec :: ParserRec Parser t a
    , parserMeta :: Meta a
    , parserChanged :: Bool 
    , parserFrozen :: Bool
    } -> Parser t a

data ParserRec p t a where
  Alt :: (Ord t, Ord a)        => p t a -> p t a -> ParserRec p t a
  Con :: (Ord t, Ord a, Ord b) => p t a -> p t b -> ParserRec p t (a, b)
  Red :: (Ord t, Ord a, Ord b) => p t a -> (Set a -> Set b) -> ParserRec p t b
  Ter :: (Ord t)               => Set t -> ParserRec p t t
  Eps :: (Ord t, Ord a)        => Set a -> ParserRec p t a
  Emp :: (Ord t, Ord a)        => ParserRec p t a

-- Parser Meta Information

data Meta a = Meta 
  { metaNullable :: Bool
  , metaEmpty :: Bool
  , metaFailAll :: Bool
  , metaParseNull :: Set a
  } deriving (Eq, Ord, Show)

mapMetaParseNull :: (Ord a, Ord b) => (Set a -> Set b) -> Meta a -> Meta b
mapMetaParseNull f (Meta n e fa pn) = Meta n e fa (f pn)

topMeta :: Meta a
topMeta = Meta
  { metaNullable = False
  , metaEmpty = True
  , metaFailAll = True
  , metaParseNull = Set.empty
  }

-- Parser Builders and Combinators

nullable, empty, failAll :: Parser t a -> Bool
nullable = metaNullable . parserMeta
empty = metaEmpty . parserMeta
failAll = metaFailAll . parserMeta

parseNull :: Parser t a -> Set a
parseNull = metaParseNull . parserMeta

(<|>) :: (Ord t, Ord a) => Parser t a -> Parser t a -> Parser t a
a <|> b = Parser (Alt a b) topMeta True False

(<~>) :: (Ord t, Ord a, Ord b) => Parser t a -> Parser t b -> Parser t (a, b)
a <~> b = Parser (Con a b) topMeta True False

(==>) :: (Ord t, Ord a, Ord b) => Parser t a -> (a -> b) -> Parser t b
a ==> f = a |==> Set.map f

(|==>) :: (Ord t, Ord a, Ord b) => Parser t a -> (Set a -> Set b) -> Parser t b
a |==> f = Parser (Red a f) topMeta True False

ter :: (Ord t) => t -> Parser t t
ter = terM . Set.singleton

terM :: (Ord t) => Set t -> Parser t t
terM tM = Parser (Ter tM) meta False True
  where
    meta = Meta { metaNullable = False
                , metaEmpty = False
                , metaFailAll = False
                , metaParseNull = Set.empty
                }

eps :: (Ord t, Ord a) => a -> Parser t a
eps = epsM . Set.singleton

epsM :: (Ord t, Ord a) => Set a -> Parser t a
epsM eM = Parser (Eps eM) meta False True
  where
    meta = Meta { metaNullable = True
                , metaEmpty = False
                , metaFailAll = True
                , metaParseNull = eM
                }


emp :: (Ord t, Ord a) => Parser t a
emp = Parser Emp meta False True
  where
    meta = Meta { metaNullable = False
                , metaEmpty = True
                , metaFailAll = True
                , metaParseNull = Set.empty
                }

infixr 3 <~>
infixr 1 <|>
infix 2 ==>, |==>

-- Parsing

parse :: (Ord t) => [t] -> Parser t a -> Set a
parse [] = parseNull . computeMeta
parse (t:ts) = parse ts . parseStep t

parseStep :: (Ord t) => t -> Parser t a -> Parser t a
parseStep t = compactStep . deriveStep t

deriveStep :: (Ord t) => t -> Parser t a -> Parser t a
deriveStep t = derive t . computeMeta

compactStep :: Parser t a -> Parser t a
compactStep = compact . computeMeta

-- Derivative

derive :: (Ord t) => t -> Parser t a -> Parser t a
derive = flip $ strongRefMemo (\ p -> strongEqMemo (\ t -> derive' t p))

derive' :: t -> Parser t a -> Parser t a
derive' t p@(Parser _ _ _ _)  
  | empty p = emp
  | nullable p && failAll p = emp
  | otherwise = case parserRec p of
      (Alt a b) | empty a -> derive t b
                | empty b -> derive t a
                | otherwise -> derive t a <|> derive t b
      (Con a b) | not (nullable a) -> derive t a <~> b
                | nullable a && failAll a -> epsM (parseNull a) <~> derive t b
                | otherwise -> derive t a <~> b <|> epsM (parseNull a) <~> derive t b
      (Red a f) -> derive t a |==> f
      (Ter tM) | t `Set.member` tM -> eps t
               | otherwise -> emp
      (Eps _) -> emp
      Emp -> emp

-- Compaction

compact :: Parser t a -> Parser t a
compact = strongRefMemo compact'

compact' :: Parser t a -> Parser t a
compact' p@(Parser _ _ _ _)
  | empty p = emp
  | nullable p && failAll p = epsM $ parseNull p
  | otherwise = case parserRec p of
      (Alt a b) 
        | empty a -> compact b
        | empty b -> compact a
        | otherwise -> compact a <|> compact b
      (Con a b) 
        | nullable a && failAll a -> 
            compact b |==> (\ yM -> Set.fromList [ (x, y) | x <- Set.toList (parseNull a)
                                                          , y <- Set.toList yM 
                                                          ])
        | nullable b && failAll b ->
            compact a |==> (\ xM -> Set.fromList [ (x, y) | x <- Set.toList xM 
                                                          , y <- Set.toList (parseNull b)
                                                          ])
        | otherwise -> compact a <~> compact b
      (Red (Parser (Red a' f) _ _ _) g) -> compact a' |==> g . f
      (Red a f) -> compact a |==> f
      (Ter _) -> p
      (Eps _) -> error "impossible" -- case caught in nullable && fail all branch
      Emp -> error "impossible" -- case caught in empty branch

-- Metadata Least-Fixed-Point Computation

computeMetaStep :: Parser t a -> Parser t a
computeMetaStep = strongRefMemo computeMetaStep'

computeMetaStep' :: Parser t a -> Parser t a
computeMetaStep' p@(Parser _ _ _ _)
  | parserFrozen p = p
  | otherwise = 
      let nextMeta = case parserRec p of
            (Alt a b) -> Meta 
              { metaNullable = nullable a || nullable b
              , metaEmpty = empty a && empty b
              , metaFailAll = failAll a && failAll b
              , metaParseNull = parseNull a `Set.union` parseNull b
              }
            (Con a b) -> Meta 
              { metaNullable = nullable a && nullable b
              , metaEmpty = empty a || empty b
              , metaFailAll = failAll a && failAll b
              , metaParseNull = 
                  Set.fromList [ (x, y) | x <- Set.toList (parseNull a) , y <- Set.toList (parseNull b) ]
              } 
            (Red a f) -> mapMetaParseNull f (parserMeta a)
            (Ter _) -> error "impossible" -- case caught in frozen branch
            (Eps _) -> error "impossible" -- case caught in frozen branch
            Emp -> error "impossible" -- case caught in frozen branch
          nextRec = case parserRec p of
            (Alt a b) -> Alt (computeMetaStep a) (computeMetaStep b)
            (Con a b) -> Con (computeMetaStep a) (computeMetaStep b)
            (Red a f) -> Red (computeMetaStep a) f
            (Ter _) -> error "impossible" -- case caught in frozen branch
            (Eps _) -> error "impossible" -- case caught in frozen branch
            Emp -> error "impossible" -- case caught in frozen branch
      in Parser nextRec nextMeta (parserMeta p /= nextMeta) False

computeMeta :: Parser t a -> Parser t a
computeMeta p
  | parserFrozen p = p
  | not $ anyChanged p = freeze p
  | otherwise = computeMeta (computeMetaStep p)

anyChanged :: Parser t a -> Bool
anyChanged = parserFoldL (\ c p -> c || parserChanged p) False

-- Freezing Metadata (after lfp)

freeze :: Parser t a -> Parser t a
freeze = strongRefMemo freeze'

freeze' :: Parser t a -> Parser t a
freeze' p@(Parser _ _ _ _)
  | parserFrozen p = p
  | otherwise =
      let nextRec = case parserRec p of
            (Alt a b) -> Alt (freeze a) (freeze b)
            (Con a b) -> Con (freeze a) (freeze b)
            (Red a f) -> Red (freeze a) f
            (Ter _) -> error "impossible" -- case caught in frozen branch
            (Eps _) -> error "impossible" -- case caught in frozen branch
            Emp -> error "impossible" -- case caught in frozen branch
      in Parser nextRec (parserMeta p) (parserChanged p) True

-- Existentials

newtype ExParser = ExParser { unExParser :: forall z. (forall t a. Parser t a -> z) -> z }

exParser :: Parser t a -> ExParser
exParser p = ExParser $ \ f -> f p

runExParser :: (forall t a. Parser t a -> z) -> ExParser -> z
runExParser f ep = unExParser ep f

-- Parser Helpers

parserType :: Parser t a -> ParserRecType
parserType = parserRecType . parserRec
  where
    parserRecType (Alt _ _) = AltType
    parserRecType (Con _ _) = ConType
    parserRecType (Red _ _) = RedType
    parserRecType (Ter _)   = TerType
    parserRecType (Eps _)   = EpsType
    parserRecType Emp       = EmpType

parserChildren :: Parser t a -> [ExParser]
parserChildren = parserRecChildren . parserRec
  where
    parserRecChildren (Alt a b)  = [exParser a, exParser b]
    parserRecChildren (Con a b)  = [exParser a, exParser b]
    parserRecChildren (Red a _) = [exParser a]
    parserRecChildren (Ter _)   = []
    parserRecChildren (Eps _)   = []
    parserRecChildren Emp       = []

foldlParserChildrenM :: (Monad m) => (forall t b. c -> Parser t b -> m c) -> c -> Parser t2 a -> m c
foldlParserChildrenM f i p = foldM g i $ parserChildren p
  where
    g t gp = unExParser gp (f t)

-- Inspection

type ParserInspect b =  (forall t a. Parser t a -> IO Integer) 
                     -> (forall t a. Parser t a -> IO Bool) 
                     -> (forall t a. Parser t a -> IO b)

inspectParser :: ParserInspect b -> Parser t a -> b
inspectParser f p = unsafePerformIO $ do
  reifiedPt <- makeHashedSNTable
  seenPt    <- makeHashedSNTable
  uidPt     <- newIORef 1
  f (lookupId reifiedPt uidPt) (seenId seenPt) p
  where
    makeHashedSNTable :: IO (HashTable DynamicStableName b)
    makeHashedSNTable = HT.new (==) (fromIntegral . hashStableName . unDynamicStableName)

lookupId :: HashTable DynamicStableName Integer
         -> IORef Integer 
         -> Parser t a 
         -> IO Integer
lookupId reifiedPt uidPt p 
  | p `seq` True = do
      stblName <- makeDynStableName p
      lookupValM <- HT.lookup reifiedPt stblName
      case lookupValM of
        (Just lookupVal) -> return lookupVal
        Nothing -> do
          thisId <- readIORef uidPt
          modifyIORef uidPt (+ 1)
          HT.insert reifiedPt stblName thisId
          return thisId
  | otherwise = error "seq failed"

seenId :: HashTable DynamicStableName () -> Parser t a -> IO Bool
seenId seenPt p 
  | p `seq` True = do 
      stblName <- makeDynStableName p
      lookupValM <- HT.lookup seenPt stblName
      case lookupValM of
        (Just ()) -> return True
        Nothing -> do
          HT.insert seenPt stblName ()
          return False
  | otherwise = error "seq failed"

type ParserExtraFoldL b = forall t a. b -> Parser t a -> Integer -> Integer -> [Integer] -> b

parserExtraFoldL :: ParserExtraFoldL b -> b -> Parser t a -> b
parserExtraFoldL f' i' = inspectParser $ inspectf f' i'
  where
    inspectf :: ParserExtraFoldL b -> b -> ParserInspect b
    inspectf f i uidM isSeenM p 
      | p `seq` True = do
          isSeen <- isSeenM p
          if isSeen then return i else do
            uid <- uidM p
            cuids <- mapM (runExParser uidM) $ parserChildren p
            pid <- liftM (fromIntegral . hashStableName) $ makeStableName p
            let next = f i p uid pid cuids
            foldlParserChildrenM (\t p' -> inspectf f t uidM isSeenM p') next p
      | otherwise = error "seq failed"


type ParserFoldL b = forall t a. b -> Parser t a -> b

parserFoldL :: ParserFoldL b -> b -> Parser t a -> b
parserFoldL f i' = parserExtraFoldL (\ i p _ _ _ -> f i p) i'

-- Reification

data ParserRecType = AltType | ConType | RedType | TerType | EpsType | EmpType
  deriving (Eq, Ord, Show)

data ParserInfo = ParserInfo Integer -- uid
                             Integer -- pid
                             ParserRecType -- type
                             (Bool, Bool, Bool) -- meta
                             Bool -- changed
                             Bool -- frozen
                             [Integer] -- children

parserToGraph :: Parser t a -> [ParserInfo]
parserToGraph = reverse . parserExtraFoldL f []
  where
    f :: ParserExtraFoldL [ParserInfo]
    f others p uid pid childrenids = ParserInfo uid
                                                pid
                                                (parserType p)
                                                (nullable p, empty p, failAll p)
                                                (parserChanged p)
                                                (parserFrozen p)
                                                childrenids
                                   : others

showParserGraph :: [ParserInfo] -> String
showParserGraph ps = printf "SIZE: %s \n" (show (length ps)) ++ intercalate "\n" (map showParserGraphSingle ps)
  where
    showParserGraphSingle :: ParserInfo -> String
    showParserGraphSingle (ParserInfo uid pid ptype md c f children) = 
      printf "%-6s%-6s%-10s%-22s%-8s%-8s%-10s" 
             (show uid)
             (show pid)
             (show ptype)
             (show md)
             (show c)
             (show f)
             (show children)

parserSize :: Parser t a -> Int
parserSize = parserFoldL f 0
  where
    f :: ParserFoldL Int
    f n _ = n + 1

instance Show (Parser t a) where
  show = showParserGraph . parserToGraph

-- Demos

xsL :: Parser Char String
xsL = ter 'x' <~> xsL ==> uncurry (:) <|> eps ""

sexp :: Parser Char String
sexp = ter '(' <~> sexpList <~> ter ')' ==> (\(a,(b,c)) -> a : b ++ [c]) <|> ter 's' ==> (:[])
  where
    sexpList = sexp <~> sexpList ==> uncurry (++) <|> eps ""

regular :: Parser Char ()
regular = ter 'h' <~> ter 'e' <~> ter 'l' <~> ter 'l' <~> ter 'o' ==> (const ())

sexpInput :: String
sexpInput = "(s(ssssssssssssssssssssssssssssssssssssssssssssssssssss(s)))"
