{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module DerParser.Knot where

import Control.DeepSeq

class Knot p k where
  knot :: (Ord t, NFData a) => p k t a -> k t (p k t a)

class (Knot p k) => BasicDeriveKnot p k where
  basicDerive      :: k t (p k t a) -> t -> p k t a
  basicFixMetaStep :: k t (p k t a) -> p k t a
  basicFreezeMeta  :: k t (p k t a) -> p k t a
  basicAnyChanged  :: k t (p k t a) -> Bool
  basicWeed        :: k t (p k t a) -> p k t a
