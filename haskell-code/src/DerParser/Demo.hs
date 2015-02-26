module DerParser.Demo
  ( module DerParser.Demo
  , module DerParser
  ) where

import Control.DeepSeq
import Control.Monad
import Data.GraphViz
import Data.Set (Set)
import Data.Time
import DerParser
import qualified Data.Set as Set
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec

rep :: () -> Parser Char String
rep () = p
  where
    p = eps (Set.singleton "") <|> s
    s = ter (== 'x') <~> p ==> uncurry (:)

infixL :: () -> Parser Char String
infixL () = p
  where
    p = ter (== '1') ==> (:[]) <|> p <~> ter (== '+') <~> ter (== '1') ==> (\ (s,(l,e)) -> s ++ [l] ++ [e])

infixL1 :: () -> Parser Char String
infixL1 () = p
  where
    p = eps (Set.singleton "1") <|> p <~> ter (== '+') <~> ter (== '1') ==> (\ (a,(b,c)) -> a ++ [b] ++ [c])

infixR :: () -> Parser Char String
infixR () = p
  where
    p = t ==> (:[]) <|> t <~> ter (== '+') <~> p ==> (\ (e,(l,s)) -> [e] ++ [l] ++ s)
    t = ter (== '1')

infixLR :: () -> Parser Char String
infixLR () = p
  where
    p = t ==> (:[]) <|> p <~> ter (== '+') <~> p ==> (\ (s1,(l,s2)) -> s1 ++ [l] ++ s2)
    t = ter (== '1')

parens :: () -> Parser Char String
parens () = ps
  where
    ps = star p ==> concat
    p = ter (== '(') <~> ps <~> ter (== ')') ==> (\(a,(b,c)) -> [a] ++ b ++ [c])

scheme :: () -> Parser Char String
scheme () = expr
  where
    expr = alts
      [ ter (== '(') <~> star expr <~> ter (== ')') 
          ==> \ (a,(b,c)) -> a : concat b ++ [c]
      , symbol
      ]
    symbol = ter (== 's') ==> (:[])

parensGenFlat :: Int -> String
parensGenFlat n = concat (replicate n "()")

parensGenDeep :: Int -> String
parensGenDeep n = replicate n '(' ++ replicate n ')'

dead :: () -> Parser Char String
dead () = p
  where
    p = ter (== 'x') <~> p ==> uncurry (:)

dead1 :: () -> Parser Char String
dead1 () = p
  where
    p = ter (== 'a') <~> ter (== 'b') <~> ter (== 'c') <~> p ==> (\(a, (b, (c, ds))) -> a:b:c:ds)

dead2 :: () -> Parser Char String
dead2 () = p
  where
    p = p <~> ter (== 'x') ==> (\(a,b) -> a ++ [b])

deriveString :: (NFData b) => Parser a b -> [a] -> Parser a b
deriveString p [] = p
deriveString p (a:as) = p1 `seq` deriveString p1 as
  where
    p1 = weed (derive p a)

parse :: (NFData b) => Parser a b -> [a] -> Set b
parse p ts = metaParseNull . fpMaybe . meta . weed . fixMeta $ zipParse p ts

parseNaive :: (NFData b) => Parser a b -> [a] -> Set b
parseNaive p ts = metaParseNull . fpMaybe . meta . weed . fixMeta $ deriveString p ts

testParser :: (Int -> String) -> IO ()
testParser f =
  forM_ [1 .. 20] $ \ n -> do
    t1 <- getCurrentTime
    putStrLn . f $ n
    t2 <- getCurrentTime
    putStrLn $ "time passed: " ++ show (diffUTCTime t2 t1)

parsecParens :: Parsec.Parser String
parsecParens = ps
  where
    ps = liftM concat (Parsec.many p)
    p = do
      a <- Parsec.char '('
      b <- ps
      c <- Parsec.char ')'
      return $ a:b ++ [c]

dumpGraph :: ParserShell k t a -> FilePath -> IO (Either String FilePath)
dumpGraph p fp = runGraphvizCommand Dot (dotifyParser p) Pdf fp
