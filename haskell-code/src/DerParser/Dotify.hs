-- DerParser.Dotify exposes functionality to convert parsers into GraphiViz dot
-- formats and images.

module DerParser.Dotify where

import Control.Arrow
import DerParser.Parser
import DerParser.Reify
import Data.GraphViz hiding (Red)
import Data.Graph.Inductive
import Text.Printf

dotifyParser :: ParserShell k t a -> DotGraph Node
dotifyParser p = graphToDot params g
  where
    g = mkGraph ns es :: Gr [Attribute] [Attribute]
    (ns, es) = second concat . unzip . map nodeAndEdges . tripZip $ removedRed p :: ([(Node, [Attribute])], [(Node, Node, [Attribute])])
    tripZip (r, ts) = let (t1s, t2s) = unzip ts in zip3 (repeat r) t1s t2s
    nodeAndEdges (rootM, nodeid, deRef) = (node, nodeEdges)
      where
        node = (nodeid, nodeLabels ++ rootAttributes)
        nodeLabels = case parserRecDeRef deRef of
          (ConDeRef _ _) -> [ Shape Record
                            , Label $ RecordLabel [FlipFields [FieldLabel "Con", FlipFields [PortName (PN "Left"), PortName (PN "Right")]]]
                            ]
          (EpsDeRef pn) -> [ Label . StrLabel $ printf "Eps %s" (map (\x -> if x == '"' then ' ' else x) (show pn) )
                      ]
          other -> [ Label . StrLabel . parserRecDeRefName $ other
                   ]
        rootAttributes = case rootM of
          (Just root) -> if nodeid == root then [Color [RGB 255 0 0]] else []
          Nothing     -> []
        nodeEdges = case parserRecDeRef deRef of
          (ConDeRef aM bM) -> concat
            [ flip (maybe []) aM $ \ a -> [ (nodeid, a, [ TailPort $ LabelledPort (PN "Left") (Just South)
                                                        , HeadPort $ CompassPoint North
                                                        ])
                                          ]
            , flip (maybe []) bM $ \ b -> [ (nodeid, b, [ TailPort $ LabelledPort (PN "Right") (Just South)
                                                        , HeadPort $ CompassPoint North
                                                        ])
                                          ]
            ]
          (AltDeRef aM bM) -> concat
            [ flip (maybe []) aM $ \ a -> [ (nodeid, a, [ TailPort $ CompassPoint South
                                                        , HeadPort $ CompassPoint North
                                                        ])
                                          ]
            , flip (maybe []) bM $ \ b -> [ (nodeid, b, [ TailPort $ CompassPoint South
                                                        , HeadPort $ CompassPoint North
                                                        ])
                                          ]
            ]
          (RedDeRef aM) -> flip (maybe []) aM $ \ a -> [ (nodeid, a, [ TailPort $ CompassPoint South
                                                                     , HeadPort $ CompassPoint North
                                                                     ]) 
                                                       ]
          _ -> []
    params = nonClusteredParams { fmtNode = snd
                                , fmtEdge = \(_, _, as) -> as 
                                , globalAttributes = [ GraphAttrs [ Center True
                                                                  , Mode Hier
                                                                  , Splines SplineEdges
                                                                  , Overlap RemoveOverlaps
                                                                  ]
                                                     ]
                                }
