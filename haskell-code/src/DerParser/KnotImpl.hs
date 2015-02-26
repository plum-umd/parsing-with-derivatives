{-# LANGUAGE MultiParamTypeClasses #-}

module DerParser.KnotImpl where

import DerParser.Derivative
import DerParser.FixPoint
import DerParser.MemoMap
import DerParser.Parser
import DerParser.Weed
import DerParser.Knot

type Parser t a = ParserShell PKnot t a

data PKnot t a = PKnot
  { pknotDerive      :: t -> a
  , pknotFixMetaStep :: a
  , pknotFreezeMeta  :: a
  , pknotAnyChanged  :: Bool
  , pknotWeed        :: a
  }

instance Knot ParserShell PKnot where
  knot p = PKnot 
    { pknotDerive      = memoFun $ deriveCompute p
    , pknotFixMetaStep = fixMetaStepCompute p
    , pknotFreezeMeta  = freezeMetaCompute p
    , pknotAnyChanged  = anyChangedCompute p
    , pknotWeed        = weedCompute p
    }

instance BasicDeriveKnot ParserShell PKnot where
  basicDerive      = pknotDerive
  basicFixMetaStep = pknotFixMetaStep
  basicFreezeMeta  = pknotFreezeMeta
  basicAnyChanged  = pknotAnyChanged
  basicWeed        = pknotWeed
