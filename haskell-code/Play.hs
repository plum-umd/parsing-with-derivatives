module Play where

import Text.Derp


sexp1 :: () -> Parser String
sexp1 () = p
  where
    p = ter "(" <~> pl <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3) <|> ter "s"
    pl = p <~> pl ==> uncurry (++) <|> eps ""

sexpIn1 :: [Token]
sexpIn1 = map (\x -> Token x x) $ words "( s ( s ( s s ( s s s ( s s s ( s ) ( s s ) s s ) s s ) s ) s ) )"

main = putStrLn $ show $ runParse (sexp1 ()) sexpIn1

