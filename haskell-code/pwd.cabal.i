Name:                pwd
Version:             0.0.1
Description:         Parsing With Derivatives
Synopsis:            Parsing With Derivatives
License:             BSD3
License-file:        LICENSE
Author:              David Darais
Maintainer:          david.darais@gmail.com
Stability:           Experimental
Category:            Parsing
Build-type:          Simple
Cabal-version:       >=1.6

Library
  HS-Source-Dirs: src
  Exposed-Modules: PWD
  Other-Modules: DynamicStableName, RefMemo
  Build-Depends: 
      base < 5
    , containers >= 0
    , criterion == 0.5.*
    , progression == 0.5.*
    , deepseq == 1.1.*
    , txt-sushi == 0.5.*
  Ghc-Options: -Wall

-- Executable runpwd
--   HS-Source-Dirs: src
--   Main-Is: PWD.hs
--   GHC-Options: -Wall -main-is PWD

