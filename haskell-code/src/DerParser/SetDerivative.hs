{-# LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

module DerParser.SetDerivative where
{-

import Data.Set (Set)
import Data.Typeable
import DerParser.ResultType
import System.IO.Unsafe
import System.Mem.StableName
import Text.Printf
import qualified Data.Set as Set
import Control.Exception

data SetResult a where
  SetResult        :: (ResultType a)                 => Set a                        -> SetResult a
  SetResultUnion   :: (ResultType a)                 => SetResult a -> SetResult a   -> SetResult a
  SetResultProduct :: (ResultType a1, ResultType a2) => SetResult a1 -> SetResult a2 -> SetResult (a1, a2)
  SetResultRed     :: (ResultType a, ResultType b)   => (a -> b)    -> SetResult a   -> SetResult b
  SetResultHole    ::                                                                   SetResult String

deriving instance Typeable1 SetResult

instance Eq (SetResult a) where 
  (SetResult x) == (SetResult y) = x == y
  (SetResultUnion x1 x2) == (SetResultUnion y1 y2) = x1 == y1 && x2 == y2
  (SetResultProduct x1 x2) == (SetResultProduct y1 y2) = case (cast x1, cast x2) of
    (Just x1casted, Just x2casted) -> x1casted == y1 && x2casted == y2
    _                              -> False
  (SetResultRed f1 x) == (SetResultRed f2 y) = case (cast f1, cast x) of
    (Just f1Casted, Just xCasted) -> unsafePerformIO $ do
      f1Name <- makeStableName f1Casted
      f2Name <- makeStableName f2
      return (f1Name == f2Name && xCasted == y)
    _                             -> False
  SetResultHole == SetResultHole = True
  _ == _ = False

instance Show (SetResult a) where
  show (SetResult x) = printf "(SetResult %s)" (show x)
  show (SetResultUnion x y) = printf "(SetResultUnion %s %s)" (show x) (show y)
  show (SetResultProduct x y) = printf "(SetResultProduct %s %s)" (show x) (show y)
  show (SetResultRed _ x) = printf "(SetResultRed %s)" (show x)
  show SetResultHole = "SetResultHole"

setResultApply :: [String] -> SetResult a -> (Set a, [String])
setResultApply input (SetResult r) = (r, input)
setResultApply input (SetResultUnion x1 x2) = 
  let (x1result, input1) = setResultApply input x1
      (x2result, input2) = setResultApply input x2
  in assert (input1 == input2) (Set.union x1result x2result, input1)
setResultApply input (SetResultProduct x1 x2) =
  let (x1result, input1) = setResultApply input x1
      (x2result, input2) = setResultApply input1 x2
  in (Set.fromList [(a, b) | a <- Set.toList x1result, b <- Set.toList x2result], input2)
setResultApply input (SetResultRed f s) = 
  let (sresult, input1) = setResultApply input s
  in (Set.map f sresult, input1)
setResultApply (i:is) SetResultHole = (Set.singleton i, is)
setResultApply [] SetResultHole = error "too many holes for input"
-}
