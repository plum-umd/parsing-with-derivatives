{-# LANGUAGE GADTs, RankNTypes #-}

module Text.Derp
  ( -- * Data Types
    Parser, Token(..)
  , -- * Parser construction
    (<|>), (<~>), (==>), nul, ter, eps, emp
  , -- * Parser computation steps
    derive, compact, parseNull
  , -- * Full parsing and result extraction
    defaultCompactSteps, compactNum, deriveStepNum, runParseNum
  , runParseStagesNum, runParseStages
  , runParseLongestMatchNum, runParseLongestMatch
  , deriveStep, runParse
  , -- * Demos
    xsR, xsL, xsIn, parens, parensIn, amb, ambIn, sexp, sexpIn
  , someStuff, someStuffG
  ) where

import Data.Maybe
import Control.Monad
import Data.Char
import Data.Function
import Data.IORef
import Data.List
import Data.Map (Map)
import System.IO.Unsafe
import System.Mem.StableName
import Text.Printf
import Unsafe.Coerce
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Represents both a formal context-free language and the
--   reduction of a member of that language to a value of type `a'.

--   Languages range of `Token' values.

data Parser t a = Parser
  { parserRec      :: ParserRec Parser t a
  , parserNullable :: FPValue Bool
  , parserEmpty    :: FPValue Bool
  , parserDerive   :: Token t -> Parser t a
  , parserCompact  :: Parser t a
  }

data ParserRec p t a where
  Alt :: (Ord t, Ord a)        => p t a -> p t a -> ParserRec p t a
  Con :: (Ord t, Ord a, Ord b) => p t a -> p t b -> ParserRec p t (a, b)
  Red :: (Ord t, Ord a, Ord b) => (Set a -> Set b) -> p t a -> ParserRec p t b
  Nul :: (Ord t, Ord a)        => p t a -> ParserRec p t a
  Zip :: (Ord t, Ord a, Ord b) => p t a -> ContextR p t a b -> ParserRec p t b
  Ter :: (Ord t)               => Set t -> ParserRec p t String
  Eps :: (Ord t, Ord a)        => Set a -> ParserRec p t a
  Emp :: (Ord t, Ord a)        => ParserRec p t a

data ContextR p t a b where
  ConContext :: (Ord t, Ord a, Ord b) => p t b -> ContextR p t (a, b) c -> ContextR p t a c
  RedContext :: (Ord t, Ord a, Ord b) => (Set a -> Set b) -> ContextR p t b c -> ContextR p t a c
  TopContext :: (Ord t, Ord a)        => ContextR p t a a

type Context t a b = ContextR Parser t a b

data Token t = Token { tokenClass :: t, tokenValue :: String }
  deriving (Eq, Ord, Show)

-- | The input type for parsing.  For example the parser:
--
-- > ter "x"
--
--   will parse:
--
-- > Token "x" "foo"
--
--   into:
--
-- > eps "foo"

parser :: (Ord t, Ord a) => ParserRec Parser t a -> FPValue Bool -> FPValue Bool -> Parser t a
parser p n e = fix $ \ self -> Parser p n e (memoFun (deriveImp self)) (compactImp self)

-- | Alternation.
(<|>) :: (Ord t, Ord a) => Parser t a -> Parser t a -> Parser t a
(<|>) a b = parser (Alt a b) FPUndecided FPUndecided
-- | Concatenation.
(<~>) :: (Ord t, Ord a, Ord b) => Parser t a -> Parser t b -> Parser t (a, b)
(<~>) a b = parser (Con a b) FPUndecided FPUndecided
-- | Reduction.
(==>) :: (Ord t, Ord a, Ord b) => Parser t a -> (a -> b) -> Parser t b
(==>) p f = p ==>| Set.map f
-- | Set generalized version of `==>'.
(==>|) :: (Ord t, Ord a, Ord b) => Parser t a -> (Set a -> Set b) -> Parser t b
(==>|) p f = parser (Red f p) FPUndecided FPUndecided
-- | Null-parse extraction.
nul :: (Ord t, Ord a) => Parser t a -> Parser t a
nul p = parser (Nul p) FPUndecided FPUndecided
-- | One-hole-context focus.
pzip :: (Ord t, Ord a, Ord b) => Parser t a -> Context t a b -> Parser t b
pzip p c = parser (Zip p c) (FPDecided False) (FPDecided False)
-- | Terminal.
ter :: (Ord t) => t -> Parser t String
ter = terM . Set.singleton
-- | Set generalized version of `ter'.
terM :: (Ord t) => Set t -> Parser t String
terM tM = parser (Ter tM) (FPDecided False) (FPDecided False)
-- | Epsilon/empty-string.
eps :: (Ord t, Ord a) => a -> Parser t a
eps = epsM . Set.singleton
-- | Set generalized version of `eps'.
epsM :: (Ord t, Ord a) => Set a -> Parser t a
epsM e = parser (Eps e) (FPDecided True) (FPDecided False)
-- | The empty language.
emp :: (Ord t, Ord a) => Parser t a
emp = parser Emp (FPDecided False) (FPDecided True)

infixr 3 <~>
infixr 1 <|>
infix 2 ==>, ==>|

-- | Kleene Star
star :: (Ord t, Ord a) => Parser t a -> Parser t [a]
star p = r
  where
    r = eps [] <|> p <~> r ==> uncurry (:)

star1 :: (Ord t, Ord a) => Parser t a -> Parser t [a]
star1 p = p <~> star p ==> uncurry (:)

option :: (Ord t, Ord a) => Parser t a -> Parser t (Maybe a)
option p = r
  where
    r = eps Nothing <|> p ==> Just

terS :: (Ord t) => [t] -> Parser t String
terS ts = m ts ==> concat
  where
    m [] = eps []
    m (a:as) = ter a <~> m as ==> uncurry (:)

-- | The main derivative function.

derive :: Parser t a -> Token t -> Parser t a
derive = parserDerive

deriveImp :: Parser t a -> Token t -> Parser t a
deriveImp p' x' = deriveImpRec (parserRec p') x'
  where
    deriveImpRec (Alt a b) x = derive a x <|> derive b x
    deriveImpRec (Con a b) x = derive a x <~> b <|> nul a <~> derive b x
    deriveImpRec (Red f a) x = derive a x ==>| f
    deriveImpRec (Nul _) _ = emp
    deriveImpRec (Zip p c) t = pzip (derive p t) c 
    deriveImpRec (Ter c) (Token x t) | x `Set.member` c = eps t | otherwise = emp
    deriveImpRec (Eps _) _ = emp
    deriveImpRec Emp _ = emp

-- | The optimization step of the algorithm.

compact :: Parser t a -> Parser t a
compact = parserCompact

compactImp :: (Ord t, Ord a) => Parser t a -> Parser t a
compactImp p = compactImpRec $ parserRec p
  where
    compactImpRec (Alt (Parser Emp _ _ _ _) (Parser Emp _ _ _ _)) = emp
    compactImpRec (Alt (Parser Emp _ _ _ _) b) = compact b
    compactImpRec (Alt a (Parser Emp _ _ _ _)) = compact a
    compactImpRec (Alt (Parser (Eps sM) _ _ _ _) (Parser (Eps tM) _ _ _ _)) = epsM (sM `Set.union` tM)
    compactImpRec (Alt a b) = (compact a <|> compact b) 
      { parserNullable = parserNullable a <||> parserNullable b 
      , parserEmpty = parserEmpty a <&&> parserEmpty b
      }
    compactImpRec (Con (Parser Emp _ _ _ _) _) = emp
    compactImpRec (Con _ (Parser Emp _ _ _ _)) = emp
    compactImpRec (Con (Parser (Eps sM) _ _ _ _) b) = compact b ==>| (\ xM -> Set.fromList [ (s, x) | s <- Set.toList sM, x <- Set.toList xM ])
    compactImpRec (Con a (Parser (Eps sM) _ _ _ _)) = compact a ==>| (\ xM -> Set.fromList [ (x, s) | x <- Set.toList xM, s <- Set.toList sM ])
    -- compactImpRec (Con a b) 
    --   | parserNullable a == FPDecided False && parserNullable b == FPDecided False 
    --     && parserEmpty a == FPDecided False && parserEmpty b == FPDecided False = 
    --   pzip (compact a) (ConContext (compact b) TopContext)
    compactImpRec (Con a b) = (compact a <~> compact b) 
      { parserNullable = parserNullable a <&&> parserNullable b 
      , parserEmpty = parserEmpty a <||> parserEmpty b
      }
    compactImpRec (Red _ (Parser Emp _ _ _ _)) = emp
    compactImpRec (Red f (Parser (Eps sM) _ _ _ _)) = epsM (f sM)
    compactImpRec (Red f (Parser (Red g a) _ _ _ _)) = compact a ==>| f . g
    compactImpRec (Red f a) = (compact a ==>| f) 
      { parserNullable = parserNullable a 
      , parserEmpty = parserEmpty a
      }
    compactImpRec (Nul (Parser (Con a b) _ _ _ _)) = nul (compact a) <~> nul (compact b)
    compactImpRec (Nul (Parser (Alt a b) _ _ _ _)) = nul (compact a) <|> nul (compact b)
    compactImpRec (Nul (Parser (Red f a) _ _ _ _)) = nul (compact a) ==>| f
    compactImpRec (Nul (Parser (Zip a c) _ _ _ _)) = pzip (nul a) (nulContext c)
    compactImpRec (Nul a@(Parser (Nul _) _ _ _ _)) = compact a
    compactImpRec (Nul (Parser (Eps sM) _ _ _ _)) = epsM sM
    compactImpRec (Nul (Parser (Ter _) _ _ _ _)) = emp
    compactImpRec (Nul (Parser Emp _ _ _ _)) = emp
    compactImpRec (Zip a TopContext) = compact a
    compactImpRec (Zip (Parser Emp _ _ _ _) _) = emp
    compactImpRec (Zip a c) | parserNullable a /= FPDecided False = unfoldOne (compact a) c
    compactImpRec (Zip (Parser (Zip a c) _ _ _ _) d) = pzip (compact a) (thread c d)
    compactImpRec (Zip (Parser (Red f a) _ _ _ _) c) = pzip (compact a) (RedContext f c)
    compactImpRec (Zip a c) = pzip (compact a) c
    compactImpRec (Ter _) = p
    compactImpRec (Eps sM) | sM == Set.empty = emp
    compactImpRec (Eps _) = p
    compactImpRec Emp = p

    nulContext :: Context t a b -> Context t a b
    nulContext (ConContext a c) = ConContext (nul a) (nulContext c)
    nulContext (RedContext f c) = RedContext f (nulContext c)
    nulContext TopContext = TopContext

    thread :: (Ord t, Ord a, Ord b, Ord c) => Context t a b -> Context t b c -> Context t a c
    thread TopContext d = d
    thread (RedContext f c) d = RedContext f (thread c d)
    thread (ConContext a c) d = ConContext a (thread c d)

    unfoldOne :: (Ord t, Ord a, Ord b) => Parser t a -> Context t a b -> Parser t b
    unfoldOne a (ConContext b c) = pzip (a <~> b) c
    unfoldOne a (RedContext f c) = unfoldOne (a ==>| f) c
    unfoldOne _ TopContext = error "cannot unfold top"

-- | Extract the parse-null set of a parser.

parseNull :: (Ord t, Ord a) => Parser t a -> Set a
parseNull p = work $ nul p
  where
    work (Parser (Eps sM) _ _ _ _) = sM
    work (Parser Emp _ _ _ _) = Set.empty
    work other = work $ compact other

-- running parsers

-- | A specified number of compactions.
compactNum :: Int -> Parser t a -> Parser t a
compactNum 0 p = p
compactNum n p = compactNum (n - 1) (compact p)

-- | Derivation followed by a specified number of compactions.
deriveStepNum :: Int -> Parser t a -> Token t -> Parser t a
deriveStepNum n p i = compactNum n $ derive p i

-- | Parse using a specified number of intermediate compactions.
runParseNum :: (Ord t, Ord a) => Int -> Parser t a -> [Token t] -> Set a
runParseNum _ (Parser Emp _ _ _ _) _ = Set.empty
runParseNum _ p [] = parseNull p
runParseNum n p (i:is) = runParseNum n (deriveStepNum n p i) is

-- | The number of compact steps that usually keeps a parser constant in size
--   while parsing.
defaultCompactSteps :: Int
defaultCompactSteps = 10

-- | Derivation followed by the default number of compactions.
deriveStep :: Parser t a -> Token t -> Parser t a
deriveStep = deriveStepNum defaultCompactSteps

-- | Parse using the default number of intermediate compactions.  This is the
--   main parsing function.  Examples:
--
-- > let e =     ter "num"
-- >         <|> e <~> ter "+" <~> e ==> (\(x1,(o,x2)) -> "(" ++ x1 ++ o ++ x2 ++ ")")
-- > in runParse e [Token "num" "1", Token "+" "+", Token "num" 3", Token "+" "+", Token "num" "5"]
--
-- evaluates to:
--
-- > Set.fromList ["((1+3)+5)", "(1+(3+5))"]
--
-- > let e =     ter "num" ==> read 
-- >         <|> e <~> ter "+" <~> e ==> (\(x1,(_,x2)) -> x1 + x2)
-- > in runParse e [Token "num" "1", Token "+" "+", Token "num" 3", Token "+" "+", Token "num" "5"]
--
-- evaluates to:
--
-- > Set.fromList [9]
--
runParse :: (Ord t, Ord a) => Parser t a -> [Token t] -> Set a
runParse = runParseNum defaultCompactSteps

runParseStagesNum :: (Ord t, Ord a) => Int -> Parser t a -> [Token t] -> [(Parser t a, Set a, [Token t])]
runParseStagesNum n p input = ((p, parseNull p, input) :) $
  case input of
    [] -> []
    (i:is) -> runParseStagesNum n (deriveStepNum n p i) is

runParseStages :: (Ord t, Ord a) => Parser t a -> [Token t] -> [(Parser t a, Set a, [Token t])]
runParseStages = runParseStagesNum defaultCompactSteps

runParseLongestMatchNum :: (Ord t, Ord a) => Int -> Parser t a -> [Token t] -> Maybe (Int, Set a, [Token t])
runParseLongestMatchNum n p input = findLongestMatch 0 $ runParseStagesNum n p input
  where
    findLongestMatch _ [] = Nothing
    findLongestMatch _ ((Parser Emp _ _ _ _, _, _):_) = Nothing
    findLongestMatch l ((_, np, ts):others) = case findLongestMatch (l + 1) others of
      (Just result) -> Just result
      Nothing
        | np == Set.empty -> Nothing
        | otherwise       -> Just (l, np, ts)

runParseLongestMatch :: (Ord t, Ord a) => Parser t a -> [Token t] -> Maybe (Int, Set a, [Token t])
runParseLongestMatch = runParseLongestMatchNum defaultCompactSteps

-- inspecting parsers

parserChildren :: Parser t a -> [GenParser]
parserChildren = parserRecChildren . parserRec
  where
    parserRecChildren (Con a b) = [genParser a, genParser b]
    parserRecChildren (Alt a b) = [genParser a, genParser b]
    parserRecChildren (Red _ a) = [genParser a]
    parserRecChildren (Nul a)   = [genParser a]
    parserRecChildren (Zip a _) = [genParser a]
    parserRecChildren (Ter _)   = []
    parserRecChildren (Eps _)   = []
    parserRecChildren Emp       = []

foldlParserChildrenM :: (forall t b. c -> Parser t b -> IO c) -> c -> Parser t2 a -> IO c
foldlParserChildrenM f i p = foldM g i $ parserChildren p
  where
    g t (GenParser h) = h (f t)

newtype GenParser = GenParser { unGenParser :: forall c. (forall t b. Parser t b -> c) -> c }

genParser :: Parser t a -> GenParser
genParser p = GenParser $ \ f -> f p

runGenParser :: (forall t b. Parser t b -> c) -> GenParser -> c
runGenParser f g = unGenParser g f

data ParserRecType = ConType | AltType | RedType | NulType | ZipType | TerType | EpsType | EmpType
  deriving (Eq, Ord, Show)

parserType :: Parser t a -> ParserRecType
parserType = parserRecType . parserRec
  where
    parserRecType (Con _ _) = ConType
    parserRecType (Alt _ _) = AltType
    parserRecType (Red _ _) = RedType
    parserRecType (Nul _)   = NulType
    parserRecType (Zip _ _) = ZipType
    parserRecType (Ter _)   = TerType
    parserRecType (Eps _)   = EpsType
    parserRecType Emp       = EmpType

type ParserInspect b =  (forall t a. Parser t a -> IO Integer) 
                     -> (forall t a. Parser t a -> IO Bool) 
                     -> (forall t a. Parser t a -> IO b)

inspectParser :: ParserInspect b -> Parser t a -> b
inspectParser f p = unsafePerformIO $ do
  reifiedPt <- newIORef Map.empty
  seenPt    <- newIORef Map.empty
  uidPt     <- newIORef 1
  f (lookupId reifiedPt uidPt) (seenId seenPt) p

lookupId :: IORef (Map Int [(StableName (), Integer)]) 
         -> IORef Integer 
         -> Parser t a 
         -> IO Integer
lookupId reifiedPt uidPt p 
  | p `seq` True = do
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
  | otherwise = error "seq failed"

seenId :: IORef (Map Int [(StableName (), ())]) -> Parser t a -> IO Bool
seenId seenPt p 
  | p `seq` True = do 
    stblName <- genericStableName p
    let stblNameHashed = hashStableName stblName
    lookupValM <- liftM (extraLookup stblNameHashed stblName) $ readIORef seenPt
    case lookupValM of
      (Just ()) -> return True
      Nothing -> do
        modifyIORef seenPt $ Map.insertWith (++) stblNameHashed [(stblName, ())]
        return False
  | otherwise = error "seq failed"

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

type ParserFoldL b = forall t a. b -> Parser t a -> Integer -> Integer -> [Integer] -> b

parserDeepFoldL :: ParserFoldL b -> b -> Parser t a -> b
parserDeepFoldL f i = inspectParser $ inspectf f i

inspectf :: ParserFoldL t -> t -> ParserInspect t
inspectf f i uidM isSeenM p = do
  isSeen <- isSeenM p
  if isSeen then return i else do
    uid <- uidM p
    cuids <- mapM (runGenParser uidM) $ parserChildren p
    let pid = hashStableName (unsafePerformIO (genericStableName p))
    let next = f i p uid (fromIntegral pid) cuids
    foldlParserChildrenM (\t p' -> inspectf f t uidM isSeenM p') next p

data ParserInfo = ParserInfo Integer -- uid
                             Integer -- pid
                             ParserRecType -- type
                             (FPValue Bool) -- nullable
                             [Integer] -- children

parserToGraph :: Parser t a -> [ParserInfo]
parserToGraph = reverse . parserDeepFoldL f []
  where
    f :: ParserFoldL [ParserInfo]
    f others p uid pid childrenids = ParserInfo uid
                                                pid
                                                (parserType p)
                                                (parserNullable p)
                                                childrenids
                                   : others

showParserGraph :: [ParserInfo] -> String
showParserGraph ps = printf "SIZE: %s \n" (show (length ps)) ++ intercalate "\n" (map showParserGraphSingle ps)
  where
    showParserGraphSingle :: ParserInfo -> String
    showParserGraphSingle (ParserInfo uid pid ptype n children) = 
      printf "%-6s%-6s%-10s%-10s%-10s" 
             (show uid)
             (show pid)
             (show ptype)
             (showFPBool n)
             (show children)

parserSize :: Parser t a -> Int
parserSize = parserDeepFoldL f 0
  where
    f :: ParserFoldL Int
    f n _ _ _ _ = n + 1

instance Show (Parser t a) where
  show = showParserGraph . parserToGraph

-- FPValue

data FPValue a = FPDecided a | FPUndecided
  deriving (Eq, Ord, Show)

showFPBool :: FPValue Bool -> String
showFPBool (FPDecided True) = "True"
showFPBool (FPDecided False) = "False"
showFPBool FPUndecided = "Undecided"

(<&&>) :: FPValue Bool -> FPValue Bool -> FPValue Bool
(<&&>) (FPDecided False) _ = FPDecided False
(<&&>) _ (FPDecided False) = FPDecided False
(<&&>) FPUndecided _ = FPUndecided
(<&&>) _ FPUndecided = FPUndecided
(<&&>) (FPDecided x) (FPDecided y) = FPDecided (x && y)

(<||>) :: FPValue Bool -> FPValue Bool -> FPValue Bool
(<||>) (FPDecided True) _ = FPDecided True
(<||>) _ (FPDecided True) = FPDecided True
(<||>) FPUndecided _ = FPUndecided
(<||>) _ FPUndecided = FPUndecided
(<||>) (FPDecided x) (FPDecided y) = FPDecided (x || y)

-- util


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

-- demos

xsR :: () -> Parser String String
xsR () = p
  where
    p = eps "" <|> ter "x" <~> p ==> uncurry (++)

xsL :: () -> Parser String String
xsL () = p
  where
    p = eps "" <|> p <~> ter "x" ==> uncurry (++)

xsIn :: [Token String]
xsIn = replicate 60 (Token "x" "x")

parens :: () -> Parser String String
parens () = p
  where
    p = eps "" <|> ter "(" <~> p <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3)

parensIn :: [Token String]
parensIn = replicate 80 (Token "(" "(") ++ replicate 80 (Token ")" ")")

amb :: () -> Parser String String
amb () = p
  where
    p = ter "1" <|> p <~> ter "+" <~> p ==> (\(s1,(s2,s3)) -> "(" ++ s1 ++ s2 ++ s3 ++ ")")

ambIn :: [Token String]
ambIn = intersperse (Token "+" "+") (replicate 7 (Token "1" "1"))

sexp :: () -> Parser String String
sexp () = p
  where
    p = ter "(" <~> pl <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3) <|> ter "s"
    pl = pl <~> p ==> uncurry (++) <|> eps ""

sexpIn :: [Token String]
sexpIn = map (\x -> Token x x) $ words "( s ( s ( s s ( s s s ( s s s ( s ) ( s s ) s s ) s s ) s ) s ) )"

makeSExpIn :: Int -> [Token String]
makeSExpIn n = map (\x -> Token x x) . words $ "( " ++ build n "s" ++ " )"
  where
    build 0 x = x
    build n s = build (n - 1) s'
      where
        s' = "s ( " ++ s ++ " )"

someStuff :: [Token String]
someStuff = map (\x -> Token x x) $ words "x x x x y y y x x"

someStuffG :: () -> Parser String String
someStuffG () = p
  where
    p = eps "" <|> p <~> ter "x" ==> uncurry (++)


nilsE :: () -> Parser String ()
nilsE () = expr
  where 
    expr = op <|> atom
    op = expr <~> internal ==> const ()
    atom = ter "x" ==> const ()
    internal = ter "[" <~> expr <~> ter "]" ==> const ()
      

exprIn :: Int -> [String]
exprIn n =
  foldr (.) id
        (replicate n (\s -> ("x" :) . ("[" :) . s . ("]" :)))
        ("x" :) 
        []

exprIn2 :: [String]
exprIn2 = words "x [ x ] [ x ]"

-- lexing

stepParsers :: (Ord t, Ord a) => [Parser t a] -> [Token t] -> [(Int, Set a, [Token t])]
stepParsers ps ts = catMaybes $ map (flip runParseLongestMatch ts) ps

longestFirstMatch :: [(Int, Set a, [Token t])] -> Maybe (a, [Token t])
longestFirstMatch rs = fmap extract $ foldl pick Nothing rs
  where
    pick Nothing s = Just s
    pick tM@(Just (tlen, _, _)) c@(clen, _, _) | clen > tlen = Just c
                                               | otherwise   = tM
    extract (_, res, con) = (Set.toList res !! 0, con)

fullLex :: (Show t, Ord t, Ord a) => [Parser t a] -> [Token t] -> Either String [a]
fullLex ps [] = Right []
fullLex ps ts = case longestFirstMatch (stepParsers ps ts) of
  Nothing -> Left $ printf "cannot parse: %s" (show ts)
  Just (r, ts') -> fmap (r :) $ fullLex ps ts'
                    
charToken :: Char -> Token Char
charToken c = Token c [c]

-- sizes

reportSizes :: Parser t a -> [Token t] -> String
reportSizes = reportSizesN 0

reportSizesN :: Int -> Parser t a -> [Token t] -> String
reportSizesN _ _ [] = ""
reportSizesN n p (i:is) = printf "%3s :: %s\n" (show n) (show size) ++ reportSizesN (n + 1) p' is
  where
    p' = deriveStep p i
    size = parserSize p'
