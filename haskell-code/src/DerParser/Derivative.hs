{-# LANGUAGE GADTs #-}

module DerParser.Derivative where

import DerParser.Builder
import DerParser.Parser
import qualified Data.Map as Map
import System.IO.Unsafe

derive :: Parser a -> String -> Parser a
derive p t = unsafePerformIO $ cached cell lookupVal insertVal compute p
  where
    cell      = parserKnotDerive
    lookupVal = Map.lookup t
    insertVal = Map.insert t
    compute   = return . flip deriveImp t . parserParserRec

deriveImp :: ParserRec a -> String -> Parser a
deriveImp (Con p1 p2) t = derive p1 t <~> p2 <|> pnull p1 <~> derive p2 t
deriveImp (Alt p1 p2) t = derive p1 t <|> derive p2 t
deriveImp (Red p f)   t = derive p t ==> f
deriveImp (Ter c)     t | t == c    = deps
                        | otherwise = emp
deriveImp (Eps _)     _ = emp
deriveImp Emp         _ = emp
deriveImp (Zip p c)   t = pzip (derive p t) c
deriveImp (Nul _)     _ = emp
