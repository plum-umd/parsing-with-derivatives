module Main where

import DerParser
import DerParser.Demo

main :: IO ()
main = interact $ \ input -> (++ "\n") . show . metaParseNull . fpMaybe . meta . weed . fixMeta $ deriveString (parens ()) (lines input !! 0)
