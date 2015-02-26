{-# LANGUAGE GADTs #-}

module DerParser.FixMeta where

import DerParser.FPMaybe
import DerParser.Parser
import DerParser.Reify
import System.IO.Unsafe

fixMeta :: Parser a -> Parser a
fixMeta p | not $ anyChanged p = freezeMeta p
          | otherwise = fixMeta (fixMetaStep p)

fixMetaStep :: Parser a -> Parser a
fixMetaStep = unsafePerformIO . cached cell lookupVal insertVal compute
  where
    cell      = parserKnotFixMetaStep
    lookupVal = id
    insertVal = const . Just
    compute   = fixMetaStepImp

fixMetaStepImp :: Parser a -> IO (Parser a)
-- fixMetaStepImp p | phase0 (parserPhase p) >= Frozen = return p
fixMetaStepImp p | isDecided (parserMeta p) && not (parserChanged p) = mapParserChildren fixMetaStep p
fixMetaStepImp p | isDecided (parserMeta p) = mapParserChildren fixMetaStep p { parserChanged = False }
fixMetaStepImp p =
  mapParserChildren fixMetaStep p { parserMeta = nextMeta , parserChanged = changed }
  where
    nextMetaF (Con p1 p2) = Meta { metaNullable = nullable p1 && nullable p2
                                 , metaEmpty    = empty    p1 || empty    p2
                                 , metaFailAll  = failAll  p1 && failAll  p2
                                 } 
    nextMetaF (Alt p1 p2) = Meta { metaNullable = nullable p1 || nullable p2
                                 , metaEmpty    = empty    p1 && empty    p2
                                 , metaFailAll  = failAll  p1 && failAll  p2
                                 }
    nextMetaF (Red c _)   = fpMaybe $ parserMeta c
    nextMetaF (Zip c k)   = flip contextMeta k . fpMaybe $ parserMeta c
    nextMetaF (Nul c)     = Meta { metaNullable = nullable c
                                 , metaEmpty    = not $ nullable c
                                 , metaFailAll  = True
                                 }
    nextMetaF _           = error "parser with no children should always be decided"
    nextMeta = decidedC . nextMetaF $ parserParserRec p
    shouldBeDecided = and $ map isGenParserDecided $ parserImmediateChildren p
      where
        isGenParserDecided (GenParser f) = f decided
    decidedC = if shouldBeDecided then Decided else Undecided
    changed = parserMeta p /= nextMeta

contextMeta :: Meta -> ParserContext a b -> Meta
contextMeta m (ConRHole p c) = contextMeta m1 c
  where
    m1 = Meta
      { metaNullable = nullable p && metaNullable m 
      , metaEmpty    = empty    p || metaEmpty    m 
      , metaFailAll  = failAll  p && metaFailAll  m 
      }
contextMeta m (ConLHole p c) = contextMeta m1 c
  where
    m1 = Meta
      { metaNullable = metaNullable m && nullable p
      , metaEmpty    = metaEmpty    m || empty    p
      , metaFailAll  = metaFailAll  m && failAll  p
      }
contextMeta m (RedHole _ c) = contextMeta m c
contextMeta m (NulHole c) = contextMeta m c
contextMeta m PCTop = m

freezeMeta :: Parser a -> Parser a
freezeMeta = unsafePerformIO . cached cell lookupVal insertVal compute
  where
    cell      = parserKnotFreezeMeta
    lookupVal = id
    insertVal = const . Just
    compute   = freezeMetaImp

freezeMetaImp :: Parser a -> IO (Parser a)
freezeMetaImp p | parserChanged p = error "cannot freeze parser with changes"
-- freezeMetaImp p | phase0 (parserPhase p) >= Frozen = return p
freezeMetaImp p = do
  mapParserChildren freezeMeta p { parserMeta = makeDecided $ parserMeta p
                                 , parserPhase = (parserPhase p) { phase0 = Frozen }
                                 }

anyChanged :: Parser a -> Bool
anyChanged = parserDeepFoldL f False
  where
    f :: ParserFoldL Bool
    f tally p _ _ _ _ = tally || parserChanged p
