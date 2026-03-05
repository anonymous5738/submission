-- | Graphviz rendering for global and local automata.
module Visualise
  ( globalDot
  , localDot
  , renderGlobalPng
  , renderLocalPng
  ) where

import Data.Array (assocs)
import qualified Data.Graph as G
import Data.GraphViz
  ( GraphvizOutput(Png)
  , GraphvizParams(..)
  , graphElemsToDot
  , nonClusteredParams
  , runGraphviz
  )
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Attributes.Complete
  ( Attribute(Shape, Width, Height, FontName, PenWidth)
  , Shape(Circle, DoubleCircle)
  )
import qualified Data.Text.Lazy as TL
import Data.GraphViz.Types.Canonical (DotGraph)
import qualified Data.Map.Strict as Map
import Automata
import Syntax.AST

-- | Render a global graph to a Graphviz DOT graph.
globalDot :: GlobalGraph -> DotGraph G.Vertex
globalDot gg =
  graphElemsToDot params nodes edges
  where
    params =
      nonClusteredParams
        { globalAttributes = []
        , fmtNode = fmtGlobalNode
        , fmtEdge = edgeAttrs
        }
    nodes = assocs (ggNodes gg)
    edges =
      [ (from, to, globalEdgeLabel e)
      | ((from, to), labels) <- Map.toList (ggEdgeLabels gg)
      , e <- labels
      ]

-- | Render a local graph to a Graphviz DOT graph.
localDot :: LocalGraph -> DotGraph G.Vertex
localDot lg =
  graphElemsToDot params nodes edges
  where
    params =
      nonClusteredParams
        { globalAttributes = []
        , fmtNode = fmtLocalNode
        , fmtEdge = edgeAttrs
        }
    nodes = assocs (lgNodes lg)
    edges =
      [ (from, to, localEdgeLabel e)
      | ((from, to), labels) <- Map.toList (lgEdgeLabels lg)
      , e <- labels
      ]

-- | Write a PNG for a global graph to the given path.
renderGlobalPng :: FilePath -> GlobalGraph -> IO FilePath
renderGlobalPng path graph = runGraphviz (globalDot graph) Png path

-- | Write a PNG for a local graph to the given path.
renderLocalPng :: FilePath -> LocalGraph -> IO FilePath
renderLocalPng path graph = runGraphviz (localDot graph) Png path

fmtGlobalNode :: (G.Vertex, GlobalNode) -> [Attribute]
fmtGlobalNode (v, n) =
  [ Shape (if n == GlobalEndNode then DoubleCircle else Circle)
  , Width 0.35
  , Height 0.35
  , FontName (TL.pack "Helvetica")
  , PenWidth 1.2
  , toLabel (show v)
  ]

fmtLocalNode :: (G.Vertex, LocalNode) -> [Attribute]
fmtLocalNode (v, n) =
  [ Shape (if isEnd n then DoubleCircle else Circle)
  , Width 0.35
  , Height 0.35
  , FontName (TL.pack "Helvetica")
  , PenWidth 1.2
  , toLabel (show v)
  ]
  where
    isEnd LocalEndNode = True
    isEnd _ = False

edgeAttrs :: (a, b, String) -> [Attribute]
edgeAttrs (_, _, l) =
  [ FontName (TL.pack "Helvetica")
  , toLabel l
  ]

-- | Human-readable edge text for global automata.
globalEdgeLabel :: GlobalEdgeLabel -> String
globalEdgeLabel (GlobalEdgeLabel sender receiver lbl _) =
  participantText sender ++ " → " ++ participantText receiver ++ " : " ++ labelText lbl

-- | Human-readable edge text for local automata.
localEdgeLabel :: LocalEdgeLabel -> String
localEdgeLabel (LocalEdgeLabel dir peer lbl _) =
  case dir of
    Send -> "!" ++ participantText peer ++ " : " ++ labelText lbl
    Receive -> "?" ++ participantText peer ++ " : " ++ labelText lbl

participantText :: Participant -> String
participantText (Participant p) = p

labelText :: Label -> String
labelText (Label l) = l
