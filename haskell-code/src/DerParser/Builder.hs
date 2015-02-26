{-# LANGUAGE RankNTypes #-}

module DerParser.Builder where

import Data.Function
import DerParser.FPMaybe
import DerParser.Parser
import DerParser.ResultType
import System.IO.Unsafe

metaTop :: FPMaybe Meta
metaTop = Undecided Meta 
  { metaNullable = False
  , metaEmpty    = True
  , metaFailAll  = True
  }

phaseTop :: Phase0
phaseTop = NoPhase Dirty

(<~>) :: (ResultType a1, ResultType a2) => Parser a1 -> Parser a2 -> Parser (a1, a2)
(<~>) p1 p2 = Parser
  { parserParserRec  = Con p1 p2
  , parserMeta       = metaTop
  , parserChanged    = True
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }

(<|>) :: (ResultType a) => Parser a -> Parser a -> Parser a
(<|>) p1 p2 = Parser
  { parserParserRec  = Alt p1 p2
  , parserMeta       = metaTop
  , parserChanged    = True
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }

(==>) :: (ResultType a, ResultType b) => Parser a -> (a -> b) -> Parser b
(==>) p f = Parser
  { parserParserRec  = Red p f
  , parserMeta       = metaTop
  , parserChanged    = True
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }

ter :: String -> Parser String
ter p = Parser
  { parserParserRec  = Ter p
  , parserMeta       = terMeta
  , parserChanged    = False
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }
  where
    terMeta = Decided Meta
      { metaNullable  = False
      , metaEmpty     = False
      , metaFailAll   = False
      }

eps :: (ResultType a) => a -> Parser a
eps pn = Parser
  { parserParserRec  = Eps (EpsValue pn)
  , parserMeta       = epsMeta
  , parserChanged    = False
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }
  where
    epsMeta = Decided Meta
      { metaNullable = True
      , metaEmpty    = False
      , metaFailAll  = True
      }

deps :: Parser String
deps = Parser
  { parserParserRec  = Eps EpsDValue
  , parserMeta       = depsMeta
  , parserChanged    = False
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }
  where
    depsMeta = Decided Meta
      { metaNullable = True
      , metaEmpty    = False
      , metaFailAll  = True
      }

emp :: (ResultType a) => Parser a
emp = Parser
  { parserParserRec  = Emp
  , parserMeta       = empMeta
  , parserChanged    = False
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }
  where
    empMeta = Decided Meta
      { metaNullable = False
      , metaEmpty    = True
      , metaFailAll  = True
      }

pzip :: (ResultType a, ResultType b) => Parser a -> ParserContext a b -> Parser b
pzip p c = Parser
  { parserParserRec  = Zip p c
  , parserMeta       = metaTop
  , parserChanged    = True
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }

pnull :: (ResultType a) => Parser a -> Parser a
pnull p = Parser
  { parserParserRec  = Nul p
  , parserMeta       = metaTop
  , parserChanged    = True
  , parserPhase      = phaseTop
  , parserKnotValues = unsafePerformIO initKnotValues
  }

infixr 3 <~>
infixr 1 <|>
infix 2 ==>

alts :: (ResultType a) => [Parser a] -> Parser a
alts [] = emp
alts (p:ps) = foldr (<|>) p ps

star :: (ResultType a) => Parser a -> Parser [a]
star p = fix $ \ result -> alts
  [ eps []
  , p <~> result ==> uncurry (:)
  ]

plus :: (ResultType a) => Parser a -> Parser [a]
plus p = p <~> star p ==> uncurry (:)

opt :: (ResultType a) => Parser a -> Parser (Maybe a)
opt p = eps Nothing <|> p ==> Just
