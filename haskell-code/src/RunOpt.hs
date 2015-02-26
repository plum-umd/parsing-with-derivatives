module Main where

import DerParser
import DerParser.Demo

main :: IO ()
main = interact $ \ input -> (++ "\n") . show . metaParseNull . fpMaybe . meta . weed . fixMeta $ zipParse (parens ()) (lines input !! 0)
