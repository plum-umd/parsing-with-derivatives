module DerParser.Stock where

import DerParser.Builder
import Data.List
import DerParser.Parser
import DerParser.Token

xsR :: () -> Parser String
xsR () = p
  where
    p = eps "" <|> ter "x" <~> p ==> uncurry (++)

xsL :: () -> Parser String
xsL () = p
  where
    p = eps "" <|> p <~> ter "x" ==> uncurry (++)

xsIn :: [Token]
xsIn = zipWith Token (repeat "x") (map show ([1..60] :: [Int]))

parens :: () -> Parser String
parens () = p
  where
    p = eps "" <|> ter "(" <~> p <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3)

parensIn :: [Token]
parensIn = zipWith Token (replicate 20 "(" ++ replicate 20 ")") (map show ([1..40] :: [Int]))

amb :: () -> Parser String
amb () = p
  where
    p = ter "1" <|> p <~> ter "+" <~> p ==> (\(s1,(s2,s3)) -> "(" ++ s1 ++ s2 ++ s3 ++ ")")

ambIn :: [Token]
ambIn = zipWith Token (intersperse "+" (replicate 7 "1")) (intersperse "+" $ map show ([1..] :: [Int]))
