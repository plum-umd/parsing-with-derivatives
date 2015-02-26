{-# LANGUAGE GADTs, ScopedTypeVariables #-}

module DerParser.Zip where

import DerParser.Token
import Data.Maybe
import Control.Exception.Base
import Control.Monad.State
import Data.Set (Set)
import DerParser.Builder
import DerParser.Derivative
import DerParser.FixMeta
import DerParser.Parser
import DerParser.ResultType
import DerParser.Weed
import qualified Data.Set as Set
import System.IO.Unsafe

compact :: Parser a -> KFun a
compact = unsafePerformIO . cached cell lookupVal insertVal compute 
  where
    cell      = parserKnotCompact
    lookupVal = id
    insertVal = const . Just
    compute   = return . compact1

compact1 :: forall a. Parser a -> KFun a
compact1 p = KFun f
  where
    f :: forall b. (ResultType a, ResultType b) => ParserContext a b -> IO (Parser b)
    f k = 
      let (KFun r) = compactImp p
      in r k

compactImp :: Parser a -> KFun a
compactImp p = compactImpRec (parserParserRec p)
  where
    compactImpRec (Con a b) | nullable a && failAll a =
      let (KFun f) = compact b
      in KFun $ \ k -> f (ConRHole a k)
    compactImpRec (Con a b) | not (nullable a) =
      let (KFun f) = compact a
      in KFun $ \ k -> f (ConLHole b k)
    compactImpRec (Red a g) =
      let (KFun f) = compact a
      in KFun $ \ k -> f (RedHole g k)
    compactImpRec (Nul a) =
      let (KFun f) = compact a
      in KFun $ \ k -> f (NulHole k)
    compactImpRec (Zip a c) =
      let (KFun f) = compact a
      in KFun $ \ k -> f (thread c k)
    compactImpRec _ = KFun $ \ k -> 
      case k of
        PCTop -> do 
          r <- mapParserChildren compactTop p
          return  r { parserPhase = Zipped Dirty }
        _ -> do
          r <- mapParserChildren compactTop p
          return $ (pzip r { parserPhase = Zipped Dirty } k) { parserPhase = Zipped Dirty }

thread :: (ResultType a, ResultType b, ResultType c) => ParserContext a b -> ParserContext b c -> ParserContext a c
thread (ConRHole p c) k = ConRHole p (thread c k)
thread (ConLHole p c) k = ConLHole p (thread c k)
thread (RedHole f c) k = RedHole f (thread c k)
thread (NulHole c) k = NulHole (thread c k)
thread PCTop k = k

compactTop :: Parser a -> Parser a
compactTop = unsafePerformIO . cached cell lookupVal insertVal compute
  where
    cell      = parserKnotCompactTop
    lookupVal = id
    insertVal = const . Just
    compute   = compactTopImp

compactTopImp :: Parser a -> IO (Parser a)
-- compactTopImp p | phase0Type (parserPhase p) >= ZippedType = return p
compactTopImp p@(Parser _ _ _ _ _) = 
  let (KFun f) = compact p
  in f PCTop

regress :: Parser a -> Parser a
regress = unsafePerformIO . cached cell lookupVal insertVal compute
  where
    cell      = undefined -- parserKnotRegress
    lookupVal = id
    insertVal = const . Just
    compute   = regressImp

regressImp :: Parser a -> IO (Parser a)
regressImp p = mapParserChildren regress . regressImpRec $ parserParserRec p
  where
    regressImpRec (Zip a c) | nullable a && isInLHole c = undoLHole (regress a) c
    regressImpRec _ = p
      
isInLHole :: ParserContext a b -> Bool
isInLHole = undefined

undoLHole :: Parser a -> ParserContext a b -> Parser b
undoLHole = undefined

parseNull :: Parser a -> StateT [String] Maybe (Set a)
parseNull p | not (nullable p) = lift Nothing
parseNull p = parseNullRec (parserParserRec p)
  where
    parseNullRec (Con a b) = do
      xs <- parseNull a
      ys <- parseNull b
      return $ Set.fromList [(x,y) | x <- Set.toList xs, y <- Set.toList ys ]
    parseNullRec (Alt a b) = StateT $ \ input ->
      let aResult = runStateT (parseNull a) input
          bResult = runStateT (parseNull b) input
      in case (aResult, bResult) of
        (Nothing, Nothing) -> Nothing
        (Nothing, bParse) -> bParse
        (aParse, Nothing) -> aParse
        (Just (aParse, aParseInput), Just (bParse, bParseInput)) -> 
            assert (aParseInput == bParseInput) 
          $ Just (Set.union aParse bParse, aParseInput)
    parseNullRec (Red a f) = liftM (Set.map f) $ parseNull a
    parseNullRec (Ter _) = lift Nothing
    parseNullRec (Eps (EpsValue x)) = return (Set.singleton x)
    parseNullRec (Eps EpsDValue) = StateT f
      where
        f (s:ss) = Just (Set.singleton s, ss)
        f [] = error "not enough input"
    parseNullRec Emp  = lift Nothing
    parseNullRec (Zip _ _) = parseNull (weed (fixMeta (punzip p)))
    parseNullRec (Nul a) = parseNull a

punzip :: Parser a -> Parser a
punzip p = punzipRec (parserParserRec p)
  where
    punzipRec (Zip c (ConRHole a k)) = punzip (pzip (a <~> c) k)
    punzipRec (Zip c (RedHole f k))  = punzip (pzip (c ==> f) k)
    punzipRec (Zip c (NulHole k))    = punzip (pzip (pnull c) k)
    punzipRec (Zip c PCTop)          = c
    punzipRec _                      = p

deriveParser :: (ResultType a) => Parser a -> [String] -> Parser a
deriveParser p [] = p
deriveParser p (x:xs) = deriveParser (derive (step p) x) xs

deriveCompute :: (ResultType a) => Parser a -> [Token] -> Set a
deriveCompute initialp initialts = deriveCompute' initialp (map tokenClass initialts)
  where
    deriveCompute' p [] = fst . fromMaybe (Set.empty, undefined) $ runStateT (parseNull (step p)) (map tokenValue initialts)
    deriveCompute' p (t:ts) = deriveCompute' (derive (step p) t) ts

step :: Parser a -> Parser a
step = weed . fixMeta . compactTop . weed . fixMeta

deriveStep :: Parser a -> String -> Parser a
deriveStep p t = step (derive p t)
