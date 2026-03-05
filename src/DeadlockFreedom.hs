-- | Deadlock-freedom checking for context automata.
module DeadlockFreedom
  ( DeadlockFreedomError(..)
  , DeadlockFreedomResult
  , checkDeadlockFreedom
  ) where

import Automata (ContextEdgeLabel(..), ContextGraph(..))
import qualified Data.Graph as G
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Deadlock-freedom violations at sync-reachable states.
data DeadlockFreedomError = SyncTerminalHasSingles
  { dfVertex :: !G.Vertex
  , dfEnabledSingles :: Set.Set ContextEdgeLabel
  }
  deriving (Eq, Ord, Show)

-- | Result of deadlock-freedom checking.
type DeadlockFreedomResult = Either [DeadlockFreedomError] ()

-- | Check deadlock freedom on a context graph.
--
-- For every state reachable from 'cgStart' by taking only double-sided
-- transitions:
--
-- * if no outgoing double-sided transition is enabled (sync-terminal),
-- * then no outgoing single-sided transition may be enabled.
checkDeadlockFreedom :: ContextGraph -> DeadlockFreedomResult
checkDeadlockFreedom cg =
  case violations of
    [] -> Right ()
    errs -> Left errs
  where
    outgoingBySource = collectOutgoing (cgEdgeLabels cg)
    syncAdj = collectSyncAdjacency (cgEdgeLabels cg)
    syncReachable = syncReachableFrom (cgStart cg) syncAdj

    violations =
      [ SyncTerminalHasSingles
          { dfVertex = v
          , dfEnabledSingles = singles
          }
      | v <- Set.toList syncReachable
      , let outgoing = Map.findWithDefault [] v outgoingBySource
      , let singles = Set.fromList [lbl | (_, lbl@ContextSingleEdge{}) <- outgoing]
      , let hasSync = any isSync outgoing
      , not hasSync
      , not (Set.null singles)
      ]

    isSync (_, ContextSyncEdge{}) = True
    isSync _ = False

collectOutgoing ::
  Map.Map G.Edge [ContextEdgeLabel] ->
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)]
collectOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(to, lbl)] m) acc labels)
    Map.empty

collectSyncAdjacency :: Map.Map G.Edge [ContextEdgeLabel] -> Map.Map G.Vertex [G.Vertex]
collectSyncAdjacency =
  Map.foldlWithKey' step Map.empty
  where
    step acc (from, to) labels
      | any isSync labels = Map.insertWith (++) from [to] acc
      | otherwise = acc
    isSync ContextSyncEdge{} = True
    isSync _ = False

syncReachableFrom :: G.Vertex -> Map.Map G.Vertex [G.Vertex] -> Set.Set G.Vertex
syncReachableFrom start adj = go Set.empty [start]
  where
    go seen [] = seen
    go seen (v : vs)
      | v `Set.member` seen = go seen vs
      | otherwise =
          let succs = Map.findWithDefault [] v adj
           in go (Set.insert v seen) (succs ++ vs)
