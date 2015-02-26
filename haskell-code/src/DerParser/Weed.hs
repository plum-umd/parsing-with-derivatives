{-# LANGUAGE GADTs #-}

module DerParser.Weed where

import DerParser.Builder
import DerParser.Parser
import System.IO.Unsafe

weed :: Parser a -> Parser a
weed = unsafePerformIO . cached cell lookupVal insertVal compute
  where
    cell      = parserKnotWeed
    lookupVal = id
    insertVal = const . Just
    compute   = weedImp

weedImp :: Parser a -> IO (Parser a)
-- weedImp p | phase0 (parserPhase p) >= Weeded = return p
weedImp p@(Parser r _ _ _ _) = do
  result <- weedImpRec r
  return result { parserPhase = (parserPhase result) { phase0 = Weeded } }
  where
    weedImpRec _             | empty p   = return emp
    weedImpRec (Zip c PCTop)             = return $ weed c
    weedImpRec (Alt a b)     | empty a   = return $ weed b
                             | empty b   = return $ weed a
    weedImpRec (Nul c)       | failAll c = return $ weed c
    weedImpRec _                         = mapParserChildren weed p
