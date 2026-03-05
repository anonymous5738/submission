-- | Safety checking for context automata.
module Safety
  ( SafetyError(..)
  , SafetyResult
  , checkSafety
  ) where

import Automata (ContextEdgeLabel(..), ContextGraph(..), LocalDirection(..))
import qualified Data.Graph as G
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Label, Participant)

-- | Safety violations detected at a sync-reachable context state.
data SafetyError = MissingReceiveLabel
  { seVertex :: !G.Vertex
  , seSender :: Participant
  , seReceiver :: Participant
  , seLabel :: Label
  , seEnabledReceives :: Set.Set Label
  }
  deriving (Eq, Ord, Show)

-- | Result of safety checking.
type SafetyResult = Either [SafetyError] ()

-- | Check the safety condition on sync-reachable context states.
--
-- For each state reachable from 'cgStart' by only taking
-- 'ContextSyncEdge' transitions, and each pair @(p, q)@:
--
-- if both
--
-- * at least one send @p -> q@ is enabled, and
-- * at least one receive @q <- p@ is enabled,
--
-- then every enabled send label @l@ from @p -> q@ must also appear among
-- enabled receives @q <- p@.
checkSafety :: ContextGraph -> SafetyResult
checkSafety cg =
  case concatMap checkSource (Set.toList syncReachable) of
    [] -> Right ()
    errs -> Left errs
  where
    outgoingBySource = collectOutgoing (cgEdgeLabels cg)
    syncAdj = collectSyncAdjacency (cgEdgeLabels cg)
    syncReachable = syncReachableFrom (cgStart cg) syncAdj

    checkSource :: G.Vertex -> [SafetyError]
    checkSource source =
      let outgoing = Map.findWithDefault [] source outgoingBySource
          sends = enabledSends outgoing
          recvs = enabledReceives outgoing
          pairs = Set.intersection (Map.keysSet sends) (Map.keysSet recvs)
       in concatMap (pairErrors source sends recvs) (Set.toList pairs)

pairErrors ::
  G.Vertex ->
  Map.Map (Participant, Participant) (Set.Set Label) ->
  Map.Map (Participant, Participant) (Set.Set Label) ->
  (Participant, Participant) ->
  [SafetyError]
pairErrors source sends recvs (sender, receiver) =
  [ MissingReceiveLabel
      { seVertex = source
      , seSender = sender
      , seReceiver = receiver
      , seLabel = missing
      , seEnabledReceives = recvLabels
      }
  | missing <- Set.toList (Set.difference sendLabels recvLabels)
  ]
  where
    sendLabels = Map.findWithDefault Set.empty (sender, receiver) sends
    recvLabels = Map.findWithDefault Set.empty (sender, receiver) recvs

enabledSends :: [(G.Vertex, ContextEdgeLabel)] -> Map.Map (Participant, Participant) (Set.Set Label)
enabledSends =
  foldl' step Map.empty
  where
    step acc (_, ContextSingleEdge actor Send peer label) =
      Map.insertWith Set.union (actor, peer) (Set.singleton label) acc
    step acc _ = acc

enabledReceives :: [(G.Vertex, ContextEdgeLabel)] -> Map.Map (Participant, Participant) (Set.Set Label)
enabledReceives =
  foldl' step Map.empty
  where
    step acc (_, ContextSingleEdge actor Receive peer label) =
      Map.insertWith Set.union (peer, actor) (Set.singleton label) acc
    step acc _ = acc

collectOutgoing ::
  Map.Map G.Edge [ContextEdgeLabel] ->
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)]
collectOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(to, lbl)] m) acc labels)
    Map.empty

collectSyncAdjacency ::
  Map.Map G.Edge [ContextEdgeLabel] ->
  Map.Map G.Vertex [G.Vertex]
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
