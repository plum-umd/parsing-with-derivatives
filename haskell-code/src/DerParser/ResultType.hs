{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module DerParser.ResultType where

import Data.Typeable

class (Eq a, Ord a, Show a) => ResultType a
instance (Eq a, Ord a, Show a) => ResultType a

