{-# LANGUAGE DeriveGeneric #-}

-- | Builders and graph representations for global and local protocol automata.
module Automata
  ( GlobalGraph(..)
  , GlobalNode(..)
  , RecVarHints(..)
  , GlobalEdgeLabel(..)
  , GlobalPayloadEdgeLabel(..)
  , buildGlobalGraph
  , GraphToTypeError(..)
  , globalRecVarHints
  , globalGraphToType
  , globalPayloadOutgoing
  , LocalGraph(..)
  , LocalNode(..)
  , LocalDirection(..)
  , LocalEdgeLabel(..)
  , LocalPayloadEdgeLabel(..)
  , emptyRecVarHints
  , buildLocalGraph
  , localGraphToType
  , localPayloadOutgoing
  , ContextGraph(..)
  , ContextState(..)
  , ContextEdgeLabel(..)
  , ContextInvariantError(..)
  , buildContextGraph
  , checkContextSynchrony
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Fix (mfix)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Array (array, assocs)
import Data.Foldable (foldl', for_)
import GHC.Generics (Generic)
import Data.List (sortOn)
import qualified Data.Graph as G
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Env
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Syntax.AST

-- | Internal state for incrementally building graphs.
data GraphBuilder node edge = GraphBuilder
  { gbNext :: !G.Vertex
  , gbNodes :: Map.Map G.Vertex node
  , gbEdges :: [(G.Edge, edge)]
  }

emptyBuilder :: GraphBuilder node edge
emptyBuilder = GraphBuilder 0 Map.empty []

freshNode :: node -> State (GraphBuilder node edge) G.Vertex
freshNode label = do
  v <- gets gbNext
  modify $ \s ->
    s
      { gbNext = v + 1
      , gbNodes = Map.insert v label (gbNodes s)
      }
  pure v

addEdge :: G.Vertex -> G.Vertex -> edge -> State (GraphBuilder node edge) ()
addEdge from to label =
  modify $ \s -> s {gbEdges = ((from, to), label) : gbEdges s}

lookupVar :: Env.Map TypeVar G.Vertex -> TypeVar -> State (GraphBuilder node edge) G.Vertex
lookupVar env var =
  case Env.lookup var env of
    Just v -> pure v
    Nothing -> error ("Unbound recursion variable: " <> show (getTypeVar var))

graphBounds :: GraphBuilder node edge -> (G.Vertex, G.Vertex)
graphBounds builder
  | gbNext builder <= 0 = error "Automata: no vertices generated"
  | otherwise = (0, gbNext builder - 1)

-- Global graphs

-- | Node kind in a global automaton.
data GlobalNode
  = GlobalNode
  | GlobalPayloadNode
  | GlobalEndNode
  deriving (Eq, Show, Generic)

instance NFData GlobalNode

-- | Edge label in a global automaton.
--
-- Includes sender/receiver and branch label, plus variable hints attached to
-- the target expression reached by this transition.
data RecVarHints = RecVarHints
  { rvhBinders :: [TypeVar]
  , rvhPreferredVar :: Maybe TypeVar
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData RecVarHints

emptyRecVarHints :: RecVarHints
emptyRecVarHints = RecVarHints [] Nothing

hasAnyRecVarHints :: RecVarHints -> Bool
hasAnyRecVarHints hints =
  not (null (rvhBinders hints))
    || case rvhPreferredVar hints of
      Just _ -> True
      Nothing -> False

recVarHintsToList :: RecVarHints -> [TypeVar]
recVarHintsToList hints =
  rvhBinders hints
    ++ case rvhPreferredVar hints of
      Just tv -> [tv]
      Nothing -> []

normaliseRecVarHints :: RecVarHints -> RecVarHints
normaliseRecVarHints hints =
  hints {rvhBinders = dedupeHints (rvhBinders hints)}

addBinderHint :: TypeVar -> RecVarHints -> RecVarHints
addBinderHint tv hints =
  normaliseRecVarHints hints {rvhBinders = rvhBinders hints ++ [tv]}

data GlobalEdgeLabel = GlobalEdgeLabel
  { geSender :: Participant
  , geReceiver :: Participant
  , geLabel :: Label
  , geTargetHints :: RecVarHints
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData GlobalEdgeLabel

data GlobalPayloadEdgeLabel = GlobalPayloadEdgeLabel
  { gpeSender :: Participant
  , gpeReceiver :: Participant
  , gpePayload :: PayloadType
  , gpeTargetHints :: RecVarHints
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData GlobalPayloadEdgeLabel

-- | Concrete graph representation for a global protocol type.
data GlobalGraph = GlobalGraph
  { ggGraph :: G.Graph
  , ggStart :: G.Vertex
  , ggNodes :: G.Table GlobalNode
  , ggEdgeLabels :: Map.Map G.Edge [GlobalEdgeLabel]
  , ggPayloadEdges :: Map.Map G.Edge [GlobalPayloadEdgeLabel]
  , ggStartVarHints :: RecVarHints
  }
  deriving (Eq, Show, Generic)

instance NFData GlobalGraph

-- | Build a global automaton from a global type.
buildGlobalGraph :: GlobalType -> GlobalGraph
buildGlobalGraph gType =
  let (start, builder) = runState (globalNode Env.empty gType) emptyBuilder
      startHints = normaliseRecVarHints (entryHintsGlobal gType)
   in finaliseGlobal start startHints builder

-- | Errors produced while reconstructing syntax trees from automata.
data GraphToTypeError
  = GraphToTypeNotImplemented
  | GraphToTypeInvalidGraph String
  deriving (Eq, Ord, Show, Generic)

instance NFData GraphToTypeError

-- | Compute the recursion-variable hints used for reconstruction.
--
-- Hints are gathered from:
--
-- * 'ggStartVarHints' for the start vertex
-- * transition hints ('geTargetHints') on incoming edges
--
-- The resulting map is deduplicated per vertex and omits empty entries.
globalRecVarHints :: GlobalGraph -> Map.Map G.Vertex [TypeVar]
globalRecVarHints gg = normaliseHintMap combined
  where
    completed = completeGlobalHints gg
    fromStart =
      if not (hasAnyRecVarHints (ggStartVarHints completed))
        then Map.empty
        else Map.singleton (ggStart completed) (recVarHintsToList (ggStartVarHints completed))
    fromEdges =
      foldl' addEdgeHints Map.empty (globalTransitions completed)
    combined = Map.unionWith (++) fromStart fromEdges

    addEdgeHints acc (_, to, _, hints) =
      if not (hasAnyRecVarHints hints)
        then acc
        else Map.insertWith (++) to (recVarHintsToList hints) acc

type TransitionId = (G.Vertex, G.Vertex, Either Label PayloadType)

data HintCompletionState = HintCompletionState
  { hcSeen :: Set.Set G.Vertex
  , hcPathEntry :: Map.Map G.Vertex (Maybe TransitionId)
  , hcUsedNames :: Set.Set String
  , hcNextIx :: !Int
  , hcStartHints :: RecVarHints
  , hcTransitionHints :: Map.Map TransitionId RecVarHints
  }

completeGlobalHints :: GlobalGraph -> GlobalGraph
completeGlobalHints gg =
  applyTransitionHints
    (gg { ggStartVarHints = hcStartHints finalState' })
    (hcTransitionHints finalState')
  where
    transitions = globalTransitions gg
    hintsByTransition = Map.fromList [((from, to, content), hints) | (from, to, content, hints) <- transitions]
    adjacency = buildAdjacency transitions
    usedNames = allHintNames (ggStartVarHints gg) transitions
    initial =
      HintCompletionState
        { hcSeen = Set.empty
        , hcPathEntry = Map.singleton (ggStart gg) Nothing
        , hcUsedNames = usedNames
        , hcNextIx = 1
        , hcStartHints = ggStartVarHints gg
        , hcTransitionHints = hintsByTransition
        }
    -- Precompute which vertices are in non-trivial SCCs (participate in cycles)
    sccCyclic = sccCyclicVertices transitions
    finalState = dfsComplete sccCyclic adjacency (ggStart gg) initial
    -- Propagate binders to ALL non-self-loop edges targeting cyclic vertices.
    -- The DFS only places binders on its discovery edges, but graphToType
    -- explores all paths, so it can reach cyclic vertices via other edges.
    cyclicVars = buildCyclicVarMap (ggStart gg) (hcStartHints finalState) (hcTransitionHints finalState) transitions
    finalState' = propagateGlobalCyclicBinders (ggStart gg) transitions cyclicVars finalState

-- | Compute the set of vertices that participate in non-trivial SCCs (cycles).
sccCyclicVertices ::
  [(G.Vertex, G.Vertex, Either Label PayloadType, RecVarHints)] ->
  Set.Set G.Vertex
sccCyclicVertices transitions =
  let adj = foldl' (\acc (from, to, _, _) -> Map.insertWith (++) from [to] acc) Map.empty transitions
      allVerts = Set.toList $ foldl' (\s (from, to, _, _) -> Set.insert from (Set.insert to s)) Set.empty transitions
      sccs = G.stronglyConnComp [(v, v, Map.findWithDefault [] v adj) | v <- allVerts]
   in Set.fromList $ concatMap extractCyclic sccs
  where
    extractCyclic (G.AcyclicSCC _) = []
    extractCyclic (G.CyclicSCC vs) = vs

dfsComplete ::
  Set.Set G.Vertex ->
  Map.Map G.Vertex [(TransitionId, G.Vertex)] ->
  G.Vertex ->
  HintCompletionState ->
  HintCompletionState
dfsComplete sccCyclic adjacency v state
  | v `Set.member` hcSeen state = state
  | otherwise =
      let entered = state {hcSeen = Set.insert v (hcSeen state)}
          succs = Map.findWithDefault [] v adjacency
          afterSuccs = foldl' step entered succs
       in afterSuccs {hcPathEntry = Map.delete v (hcPathEntry afterSuccs)}
  where
    step acc (transitionId, succV)
      | succV `Map.member` hcPathEntry acc =
          ensureEntryHintForAncestor succV acc
      | succV `Set.member` hcSeen acc =
          -- Cross-edge to already-visited vertex.  If succV is in a cycle
          -- (non-trivial SCC), globalGraphToType's all-paths DFS may enter
          -- it fresh from a different path and discover the cycle.  Ensure
          -- the cross-edge transition carries a binder for succV.
          if succV `Set.member` sccCyclic
            then ensureCrossEdgeCycleHint succV transitionId acc
            else acc
      | otherwise =
          dfsComplete
            sccCyclic
            adjacency
            succV
            (acc {hcPathEntry = Map.insert succV (Just transitionId) (hcPathEntry acc)})

ensureEntryHintForAncestor :: G.Vertex -> HintCompletionState -> HintCompletionState
ensureEntryHintForAncestor ancestor state =
  case Map.lookup ancestor (hcPathEntry state) of
    Nothing -> state
    Just Nothing ->
      if not (null (rvhBinders (hcStartHints state)))
        then state
        else case rvhPreferredVar (hcStartHints state) of
          Just tv ->
            state {hcStartHints = addBinderHint tv (hcStartHints state)}
          Nothing ->
            let (tv, state') = freshHint state
             in state' {hcStartHints = addBinderHint tv (hcStartHints state')}
    Just (Just tid) ->
      let existing = Map.findWithDefault emptyRecVarHints tid (hcTransitionHints state)
       in if not (null (rvhBinders existing))
            then state
            else case rvhPreferredVar existing of
              Just tv ->
                state
                  { hcTransitionHints =
                      Map.insert tid (addBinderHint tv existing) (hcTransitionHints state)
                  }
              Nothing ->
                let (tv, state') = freshHint state
                 in state'
                      { hcTransitionHints =
                          Map.insert tid (addBinderHint tv existing) (hcTransitionHints state')
                      }

-- | Handle a cross-edge to an already-visited vertex that is in a cycle.
-- The cross-edge transition itself needs a binder so that globalGraphToType
-- can introduce a rec variable when entering succV from this new path.
ensureCrossEdgeCycleHint :: G.Vertex -> TransitionId -> HintCompletionState -> HintCompletionState
ensureCrossEdgeCycleHint _succV transitionId state =
  let existing = Map.findWithDefault emptyRecVarHints transitionId (hcTransitionHints state)
   in if not (null (rvhBinders existing))
        then state
        else case rvhPreferredVar existing of
          Just tv ->
            state
              { hcTransitionHints =
                  Map.insert transitionId (addBinderHint tv existing) (hcTransitionHints state)
              }
          Nothing ->
            let (tv, state') = freshHint state
             in state'
                  { hcTransitionHints =
                      Map.insert transitionId (addBinderHint tv existing) (hcTransitionHints state')
                  }

freshHint :: HintCompletionState -> (TypeVar, HintCompletionState)
freshHint state =
  let (tv, nextIx', used') = freshSynthetic (hcUsedNames state) (hcNextIx state)
   in ( tv
      , state
          { hcUsedNames = used'
          , hcNextIx = nextIx'
          }
      )

-- | Build a map from cyclic vertex -> assigned TypeVar.
-- A vertex is cyclic if the start hints have a binder for it, or if
-- some transition targeting it acquired a binder during hint completion.
buildCyclicVarMap ::
  G.Vertex ->
  RecVarHints ->
  Map.Map TransitionId RecVarHints ->
  [(G.Vertex, G.Vertex, Either Label PayloadType, RecVarHints)] ->
  Map.Map G.Vertex TypeVar
buildCyclicVarMap startV startHints transHints transitions =
  let startEntry = case rvhBinders startHints of
        (tv : _) -> Map.singleton startV tv
        []       -> Map.empty
      transEntries = foldl' checkTrans Map.empty transitions
   in Map.union startEntry transEntries
  where
    checkTrans acc (from, to, content, _) =
      let tid = (from, to, content)
       in case Map.lookup tid transHints of
            Just hints | (tv : _) <- rvhBinders hints ->
              Map.insertWith (\_ old -> old) to tv acc  -- keep first
            _ -> acc

-- | Propagate binders to edges targeting cyclic vertices, but only when the
-- edge can be a tree-edge in graphToType's all-paths traversal.  Edge U→V is
-- always a back-edge when V dominates U (every path from root to U goes
-- through V).  We skip those by checking if U is reachable from root in the
-- graph with V removed.
propagateGlobalCyclicBinders ::
  G.Vertex ->
  [(G.Vertex, G.Vertex, Either Label PayloadType, RecVarHints)] ->
  Map.Map G.Vertex TypeVar ->
  HintCompletionState ->
  HintCompletionState
propagateGlobalCyclicBinders startV transitions cyclicVars state =
  foldl' propagate state transitions
  where
    -- Simple adjacency for reachability (vertex → [successor vertices])
    simpleAdj = foldl' (\acc (from, to, _, _) -> Map.insertWith (++) from [to] acc) Map.empty transitions
    -- Precompute: for each cyclic vertex V, which vertices are reachable from root without V
    reachSets = Map.mapWithKey (\v _ -> reachableWithout simpleAdj startV v) cyclicVars

    propagate acc (from, to, content, _)
      | from == to = acc  -- self-loops are always back-edges
      | otherwise =
          case Map.lookup to cyclicVars of
            Nothing -> acc
            Just tv ->
              -- Only add binder if 'from' is reachable from root without 'to'
              -- (meaning 'to' does not dominate 'from', so this edge can be a tree-edge)
              case Map.lookup to reachSets of
                Nothing -> acc
                Just reachable | not (from `Set.member` reachable) -> acc
                Just _ ->
                  let tid = (from, to, content)
                      existing = Map.findWithDefault emptyRecVarHints tid (hcTransitionHints acc)
                   in if not (null (rvhBinders existing))
                        then acc
                        else acc { hcTransitionHints = Map.insert tid (addBinderHint tv existing) (hcTransitionHints acc) }

-- | BFS to find all vertices reachable from 'root' without going through 'excluded'.
reachableWithout :: Map.Map G.Vertex [G.Vertex] -> G.Vertex -> G.Vertex -> Set.Set G.Vertex
reachableWithout adj root excluded = go Set.empty (Seq.singleton root)
  where
    go visited queue = case Seq.viewl queue of
      Seq.EmptyL -> visited
      v Seq.:< rest
        | v == excluded || v `Set.member` visited -> go visited rest
        | otherwise ->
            let visited' = Set.insert v visited
                succs = Map.findWithDefault [] v adj
             in go visited' (rest Seq.>< Seq.fromList succs)

buildAdjacency ::
  [(G.Vertex, G.Vertex, Either Label PayloadType, RecVarHints)] ->
  Map.Map G.Vertex [(TransitionId, G.Vertex)]
buildAdjacency transitions =
  foldl' add Map.empty transitions
  where
    add acc (from, to, content, _) =
      Map.insertWith (++) from [((from, to, content), to)] acc

applyTransitionHints ::
  GlobalGraph ->
  Map.Map TransitionId RecVarHints ->
  GlobalGraph
applyTransitionHints gg hintsByTransition =
  gg
    { ggEdgeLabels = Map.mapWithKey rewriteBranch (ggEdgeLabels gg)
    , ggPayloadEdges = Map.mapWithKey rewritePayload (ggPayloadEdges gg)
    }
  where
    rewriteBranch (from, to) labels =
      fmap (\lbl -> lbl {geTargetHints = Map.findWithDefault (geTargetHints lbl) (from, to, Left (geLabel lbl)) hintsByTransition}) labels
    rewritePayload (from, to) labels =
      fmap (\lbl -> lbl {gpeTargetHints = Map.findWithDefault (gpeTargetHints lbl) (from, to, Right (gpePayload lbl)) hintsByTransition}) labels

allHintNames ::
  RecVarHints ->
  [(G.Vertex, G.Vertex, Either Label PayloadType, RecVarHints)] ->
  Set.Set String
allHintNames startHints transitions =
  Set.fromList
    ( fmap getTypeVar (recVarHintsToList startHints)
        ++ [ getTypeVar tv
           | (_, _, _, hints) <- transitions
           , tv <- recVarHintsToList hints
           ]
    )

globalTransitions :: GlobalGraph -> [(G.Vertex, G.Vertex, Either Label PayloadType, RecVarHints)]
globalTransitions gg =
  [ (from, to, Left (geLabel lbl), geTargetHints lbl)
  | ((from, to), labels) <- Map.toList (ggEdgeLabels gg)
  , lbl <- labels
  ]
  ++
  [ (from, to, Right (gpePayload lbl), gpeTargetHints lbl)
  | ((from, to), labels) <- Map.toList (ggPayloadEdges gg)
  , lbl <- labels
  ]

freshSynthetic :: Set.Set String -> Int -> (TypeVar, Int, Set.Set String)
freshSynthetic used startIx =
  let candidate = "t" ++ show startIx
   in if candidate `Set.member` used
        then freshSynthetic used (startIx + 1)
        else (TypeVar candidate, startIx + 1, Set.insert candidate used)

normaliseHintMap :: Map.Map G.Vertex [TypeVar] -> Map.Map G.Vertex [TypeVar]
normaliseHintMap =
  Map.map dedupeHints . Map.filter (not . null)

dedupeHints :: [TypeVar] -> [TypeVar]
dedupeHints hints = reverse (fst (foldl' step ([], Set.empty) hints))
  where
    step (acc, seen) tv
      | tv `Set.member` seen = (acc, seen)
      | otherwise = (tv : acc, Set.insert tv seen)

-- | Reconstruct a global type from a global automaton.
--
-- The reconstruction uses two passes:
--
-- 1. complete edge/start hint lists by adding synthetic binders where needed
--    (for recursion targets discovered by DFS back-edges)
-- 2. generate the type while:
--    * introducing binders from incoming edge/start hints
--    * emitting a variable when revisiting a vertex currently on the DFS path
globalGraphToType :: GlobalGraph -> Either GraphToTypeError GlobalType
globalGraphToType gg = buildAt Set.empty Map.empty (ggStartVarHints completed) (ggStart completed)
  where
    completed = completeGlobalHints gg
    outgoing = globalOutgoing completed
    payloadOut = globalPayloadOutgoing completed

    buildAt ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      RecVarHints ->
      G.Vertex ->
      Either GraphToTypeError GlobalType
    buildAt path activeNames incomingHints v
      | v `Set.member` path =
          buildRevisit activeNames incomingHints v
      | otherwise = do
          let activeNames' =
                foldl'
                  (\env tv -> Map.insert tv v env)
                  activeNames
                  (rvhBinders incomingHints)
          nodeType <- lookupNode v
          body <- case nodeType of
            GlobalEndNode -> buildEndNode v
            GlobalNode -> buildMessageNode (Set.insert v path) activeNames' v
            GlobalPayloadNode -> buildPayloadNode (Set.insert v path) activeNames' v
          pure (foldr GRec body (rvhBinders incomingHints))

    lookupNode :: G.Vertex -> Either GraphToTypeError GlobalNode
    lookupNode v =
      case lookup v (assocs (ggNodes completed)) of
        Just node -> Right node
        Nothing ->
          Left
            ( GraphToTypeInvalidGraph
                ("Missing node metadata for vertex " ++ show v)
            )

    buildEndNode :: G.Vertex -> Either GraphToTypeError GlobalType
    buildEndNode v =
      case Map.lookup v outgoing of
        Nothing -> Right GEnd
        Just branches ->
          Left
            ( GraphToTypeInvalidGraph
                ( "End vertex "
                    ++ show v
                    ++ " has outgoing transitions: "
                    ++ show (fmap (geLabel . fst) branches)
                )
            )

    buildMessageNode ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      G.Vertex ->
      Either GraphToTypeError GlobalType
    buildMessageNode path activeNames v =
      case Map.lookup v outgoing of
        Nothing ->
          Left
            ( GraphToTypeInvalidGraph
                ("Message vertex " ++ show v ++ " has no outgoing transitions")
            )
        Just rawBranches -> do
          let branches = sortOn (\(lbl, dst) -> (geLabel lbl, dst)) rawBranches
          (sender, receiver) <- uniquePeerPair v branches
          ensureDistinctLabels v branches
          typedBranches <- traverse (buildBranch path activeNames) branches
          case typedBranches of
            [] ->
              Left
                ( GraphToTypeInvalidGraph
                    ("Message vertex " ++ show v ++ " has an empty branch list")
                )
            b0 : bs ->
              Right (GMessage sender receiver (b0 NE.:| bs))

    buildPayloadNode ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      G.Vertex ->
      Either GraphToTypeError GlobalType
    buildPayloadNode path activeNames v =
      case Map.lookup v payloadOut of
        Nothing ->
          Left (GraphToTypeInvalidGraph
            ("Payload vertex " ++ show v ++ " has no outgoing payload transition"))
        Just [(edgeLbl, dst)] -> do
          cont <- buildAt path activeNames (gpeTargetHints edgeLbl) dst
          pure (GPayload (gpeSender edgeLbl) (gpeReceiver edgeLbl) (gpePayload edgeLbl) cont)
        Just branches ->
          Left (GraphToTypeInvalidGraph
            ("Payload vertex " ++ show v ++ " has " ++ show (length branches) ++ " outgoing transitions, expected 1"))

    buildBranch ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      (GlobalEdgeLabel, G.Vertex) ->
      Either GraphToTypeError (Label, GlobalType)
    buildBranch path activeNames (edgeLbl, dst) = do
      cont <- buildAt path activeNames (geTargetHints edgeLbl) dst
      pure (geLabel edgeLbl, cont)

    buildRevisit ::
      Map.Map TypeVar G.Vertex ->
      RecVarHints ->
      G.Vertex ->
      Either GraphToTypeError GlobalType
    buildRevisit activeNames incomingHints v =
      case pickVar of
        Nothing ->
          Left
            ( GraphToTypeInvalidGraph
                ( "Encountered cycle at vertex "
                    ++ show v
                    ++ " without a visible recursion variable hint."
                )
            )
        Just tv ->
          Right (foldr GRec (GVar tv) binders)
      where
        binders = rvhBinders incomingHints
        preferredVar = rvhPreferredVar incomingHints

        activeWithBinders =
          foldl'
            (\env tv -> Map.insert tv v env)
            activeNames
            binders

        pickVar =
          case preferredVar of
            Just tv | Map.lookup tv activeWithBinders == Just v -> Just tv
            _ ->
              case [tv | (tv, target) <- Map.toList activeWithBinders, target == v] of
                tv : _ -> Just tv
                [] -> Nothing

    uniquePeerPair ::
      G.Vertex ->
      [(GlobalEdgeLabel, G.Vertex)] ->
      Either GraphToTypeError (Participant, Participant)
    uniquePeerPair v branches =
      case Set.toList peers of
        [] ->
          Left
            ( GraphToTypeInvalidGraph
                ("Message vertex " ++ show v ++ " has no labelled outgoing branches")
            )
        [pair] -> Right pair
        _ ->
          Left
            ( GraphToTypeInvalidGraph
                ( "Message vertex "
                    ++ show v
                    ++ " mixes sender/receiver pairs: "
                    ++ show (Set.toList peers)
                )
            )
      where
        peers =
          Set.fromList
            [ (geSender lbl, geReceiver lbl)
            | (lbl, _) <- branches
            ]

    ensureDistinctLabels ::
      G.Vertex ->
      [(GlobalEdgeLabel, G.Vertex)] ->
      Either GraphToTypeError ()
    ensureDistinctLabels v branches
      | Set.size labels == length branches = Right ()
      | otherwise =
          Left
            ( GraphToTypeInvalidGraph
                ( "Message vertex "
                    ++ show v
                    ++ " has duplicate branch labels: "
                    ++ show branchLabels
                )
            )
      where
        branchLabels = fmap (geLabel . fst) branches
        labels = Set.fromList branchLabels

globalOutgoing :: GlobalGraph -> Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)]
globalOutgoing gg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty
    (ggEdgeLabels gg)

globalPayloadOutgoing :: GlobalGraph -> Map.Map G.Vertex [(GlobalPayloadEdgeLabel, G.Vertex)]
globalPayloadOutgoing gg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty
    (ggPayloadEdges gg)

globalNode ::
  Env.Map TypeVar G.Vertex ->
  GlobalType ->
  State (GraphBuilder GlobalNode (Either GlobalEdgeLabel GlobalPayloadEdgeLabel)) G.Vertex
globalNode env gtype = case gtype of
  GMessage sender receiver branches -> do
    v <- freshNode GlobalNode
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- globalNode env cont
      addEdge v dest (Left (GlobalEdgeLabel sender receiver lbl (entryHintsGlobal cont)))
    pure v
  GPayload sender receiver pt cont -> do
    v <- freshNode GlobalPayloadNode
    dest <- globalNode env cont
    addEdge v dest (Right (GlobalPayloadEdgeLabel sender receiver pt (entryHintsGlobal cont)))
    pure v
  GVar var -> lookupVar env var
  GRec var body ->
    mfix $ \start ->
      globalNode (Env.insert var start env) body
  GEnd -> freshNode GlobalEndNode

finaliseGlobal :: G.Vertex -> RecVarHints -> GraphBuilder GlobalNode (Either GlobalEdgeLabel GlobalPayloadEdgeLabel) -> GlobalGraph
finaliseGlobal start startHints builder =
  let bounds = graphBounds builder
      allEdges = gbEdges builder
      graph = G.buildG bounds (map fst allEdges)
      nodeTable = array bounds (Map.toList (gbNodes builder))
      branchEdges = [(e, lbl) | (e, Left lbl) <- allEdges]
      payloadEdges = [(e, plbl) | (e, Right plbl) <- allEdges]
   in GlobalGraph
        { ggGraph = graph
        , ggStart = start
        , ggNodes = nodeTable
        , ggEdgeLabels = collectEdges branchEdges
        , ggPayloadEdges = collectEdges payloadEdges
        , ggStartVarHints = normaliseRecVarHints startHints
        }

entryHintsGlobal :: GlobalType -> RecVarHints
entryHintsGlobal =
  go []
  where
    go binders gtype =
      case gtype of
        GRec var body -> go (var : binders) body
        GVar var ->
          normaliseRecVarHints
            ( RecVarHints
                { rvhBinders = reverse binders
                , rvhPreferredVar = Just var
                }
            )
        _ ->
          normaliseRecVarHints
            ( RecVarHints
                { rvhBinders = reverse binders
                , rvhPreferredVar = Nothing
                }
            )

-- Local graphs

-- | Local communication action direction on an edge.
data LocalDirection = Send | Receive
  deriving (Eq, Ord, Show, Generic)

instance NFData LocalDirection

-- | Edge label in a local automaton.
data LocalEdgeLabel = LocalEdgeLabel
  { leDirection :: LocalDirection
  , lePeer :: Participant
  , leLabel :: Label
  , leTargetHints :: RecVarHints
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LocalEdgeLabel

data LocalPayloadEdgeLabel = LocalPayloadEdgeLabel
  { lpeDirection :: LocalDirection
  , lpePeer :: Participant
  , lpePayload :: PayloadType
  , lpeTargetHints :: RecVarHints
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LocalPayloadEdgeLabel

-- | Node kind in a local automaton.
data LocalNode
  = LocalSendNode Participant [Label]
  | LocalRecvNode Participant [Label]
  | LocalPayloadSendNode Participant PayloadType
  | LocalPayloadRecvNode Participant PayloadType
  | LocalEndNode
  deriving (Eq, Show, Generic)

instance NFData LocalNode

-- | Concrete graph representation for a projected/local protocol type.
data LocalGraph = LocalGraph
  { lgGraph :: G.Graph
  , lgStart :: G.Vertex
  , lgNodes :: G.Table LocalNode
  , lgEdgeLabels :: Map.Map G.Edge [LocalEdgeLabel]
  , lgPayloadEdges :: Map.Map G.Edge [LocalPayloadEdgeLabel]
  , lgStartVarHints :: RecVarHints
  }
  deriving (Eq, Show, Generic)

instance NFData LocalGraph

-- | Build a local automaton from a local type.
buildLocalGraph :: LocalType -> LocalGraph
buildLocalGraph lType =
  let (start, builder) = runState (localNode Env.empty lType) emptyBuilder
      startHints = normaliseRecVarHints (entryHintsLocal lType)
   in finaliseLocal start startHints builder

-- | Reconstruct a local type from a local automaton.
localGraphToType :: LocalGraph -> Either GraphToTypeError LocalType
localGraphToType lg = buildAt Set.empty Map.empty (lgStartVarHints completed) (lgStart completed)
  where
    completed = completeLocalHints lg
    outgoing = localOutgoing completed
    localPayloadOut = localPayloadOutgoing completed

    buildAt ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      RecVarHints ->
      G.Vertex ->
      Either GraphToTypeError LocalType
    buildAt path activeNames incomingHints v
      | v `Set.member` path =
          buildRevisit activeNames incomingHints v
      | otherwise = do
          let activeNames' =
                foldl'
                  (\env tv -> Map.insert tv v env)
                  activeNames
                  (rvhBinders incomingHints)
          nodeType <- lookupNode v
          body <- case nodeType of
            LocalEndNode -> buildEndNode v
            LocalSendNode peer _ -> buildChoiceNode Send peer (Set.insert v path) activeNames' v
            LocalRecvNode peer _ -> buildChoiceNode Receive peer (Set.insert v path) activeNames' v
            LocalPayloadSendNode peer pt ->
              buildPayloadNode Send peer pt (Set.insert v path) activeNames' v
            LocalPayloadRecvNode peer pt ->
              buildPayloadNode Receive peer pt (Set.insert v path) activeNames' v
          pure (foldr LRec body (rvhBinders incomingHints))

    lookupNode :: G.Vertex -> Either GraphToTypeError LocalNode
    lookupNode v =
      case lookup v (assocs (lgNodes completed)) of
        Just node -> Right node
        Nothing ->
          Left
            ( GraphToTypeInvalidGraph
                ("Missing node metadata for local vertex " ++ show v)
            )

    buildEndNode :: G.Vertex -> Either GraphToTypeError LocalType
    buildEndNode v =
      case Map.lookup v outgoing of
        Nothing -> Right LEnd
        Just branches ->
          Left
            ( GraphToTypeInvalidGraph
                ( "End local vertex "
                    ++ show v
                    ++ " has outgoing transitions: "
                    ++ show (fmap (leLabel . fst) branches)
                )
            )

    buildChoiceNode ::
      LocalDirection ->
      Participant ->
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      G.Vertex ->
      Either GraphToTypeError LocalType
    buildChoiceNode expectedDir expectedPeer path activeNames v =
      case Map.lookup v outgoing of
        Nothing ->
          Left
            ( GraphToTypeInvalidGraph
                ("Local choice vertex " ++ show v ++ " has no outgoing transitions")
            )
        Just rawBranches -> do
          let branches = sortOn (\(lbl, dst) -> (leLabel lbl, dst)) rawBranches
          ensureUniformAction v expectedDir expectedPeer branches
          ensureDistinctLabels v branches
          typedBranches <- traverse (buildBranch path activeNames) branches
          case typedBranches of
            [] ->
              Left
                ( GraphToTypeInvalidGraph
                    ("Local choice vertex " ++ show v ++ " has an empty branch list")
                )
            b0 : bs ->
              Right $
                case expectedDir of
                  Send -> LSend expectedPeer (b0 NE.:| bs)
                  Receive -> LRecv expectedPeer (b0 NE.:| bs)

    buildPayloadNode ::
      LocalDirection ->
      Participant ->
      PayloadType ->
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      G.Vertex ->
      Either GraphToTypeError LocalType
    buildPayloadNode dir peer pt path activeNames v =
      case Map.lookup v localPayloadOut of
        Nothing ->
          Left (GraphToTypeInvalidGraph
            ("Local payload vertex " ++ show v ++ " has no outgoing payload transition"))
        Just [(edgeLbl, dst)] -> do
          cont <- buildAt path activeNames (lpeTargetHints edgeLbl) dst
          pure $ case dir of
            Send -> LPayloadSend peer pt cont
            Receive -> LPayloadRecv peer pt cont
        Just branches ->
          Left (GraphToTypeInvalidGraph
            ("Local payload vertex " ++ show v ++ " has " ++ show (length branches) ++ " transitions, expected 1"))

    buildBranch ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      (LocalEdgeLabel, G.Vertex) ->
      Either GraphToTypeError (Label, LocalType)
    buildBranch path activeNames (edgeLbl, dst) = do
      cont <- buildAt path activeNames (leTargetHints edgeLbl) dst
      pure (leLabel edgeLbl, cont)

    buildRevisit ::
      Map.Map TypeVar G.Vertex ->
      RecVarHints ->
      G.Vertex ->
      Either GraphToTypeError LocalType
    buildRevisit activeNames incomingHints v =
      case pickVar of
        Nothing ->
          Left
            ( GraphToTypeInvalidGraph
                ( "Encountered local cycle at vertex "
                    ++ show v
                    ++ " without a visible recursion variable hint."
                )
            )
        Just tv ->
          Right (foldr LRec (LVar tv) binders)
      where
        binders = rvhBinders incomingHints
        preferredVar = rvhPreferredVar incomingHints

        activeWithBinders =
          foldl'
            (\env tv -> Map.insert tv v env)
            activeNames
            binders

        pickVar =
          case preferredVar of
            Just tv | Map.lookup tv activeWithBinders == Just v -> Just tv
            _ ->
              case [tv | (tv, target) <- Map.toList activeWithBinders, target == v] of
                tv : _ -> Just tv
                [] -> Nothing

    ensureUniformAction ::
      G.Vertex ->
      LocalDirection ->
      Participant ->
      [(LocalEdgeLabel, G.Vertex)] ->
      Either GraphToTypeError ()
    ensureUniformAction v expectedDir expectedPeer branches =
      if all matches branches
        then Right ()
        else
          Left
            ( GraphToTypeInvalidGraph
                ( "Local choice vertex "
                    ++ show v
                    ++ " mixes direction/peer actions."
                )
            )
      where
        matches (lbl, _) =
          leDirection lbl == expectedDir
            && lePeer lbl == expectedPeer

    ensureDistinctLabels ::
      G.Vertex ->
      [(LocalEdgeLabel, G.Vertex)] ->
      Either GraphToTypeError ()
    ensureDistinctLabels v branches
      | Set.size labels == length branches = Right ()
      | otherwise =
          Left
            ( GraphToTypeInvalidGraph
                ( "Local choice vertex "
                    ++ show v
                    ++ " has duplicate branch labels: "
                    ++ show branchLabels
                )
            )
      where
        branchLabels = fmap (leLabel . fst) branches
        labels = Set.fromList branchLabels

type LocalTransitionId = (G.Vertex, G.Vertex, LocalDirection, Participant, Either Label PayloadType)

data LocalHintCompletionState = LocalHintCompletionState
  { lhcSeen :: Set.Set G.Vertex
  , lhcPathEntry :: Map.Map G.Vertex (Maybe LocalTransitionId)
  , lhcUsedNames :: Set.Set String
  , lhcNextIx :: !Int
  , lhcStartHints :: RecVarHints
  , lhcTransitionHints :: Map.Map LocalTransitionId RecVarHints
  }

completeLocalHints :: LocalGraph -> LocalGraph
completeLocalHints lg =
  applyLocalTransitionHints
    (lg { lgStartVarHints = lhcStartHints finalState' })
    (lhcTransitionHints finalState')
  where
    transitions = localTransitions lg
    hintsByTransition = Map.fromList [(tid, hints) | (_, _, tid, hints) <- transitions]
    adjacency = buildLocalAdjacency transitions
    usedNames = allLocalHintNames (lgStartVarHints lg) transitions
    initial =
      LocalHintCompletionState
        { lhcSeen = Set.empty
        , lhcPathEntry = Map.singleton (lgStart lg) Nothing
        , lhcUsedNames = usedNames
        , lhcNextIx = 1
        , lhcStartHints = lgStartVarHints lg
        , lhcTransitionHints = hintsByTransition
        }
    finalState = dfsCompleteLocal adjacency (lgStart lg) initial
    -- Propagate binders to ALL non-self-loop edges targeting cyclic vertices.
    cyclicVars = buildLocalCyclicVarMap (lgStart lg) (lhcStartHints finalState) (lhcTransitionHints finalState) transitions
    finalState' = propagateLocalCyclicBinders (lgStart lg) transitions cyclicVars finalState

dfsCompleteLocal ::
  Map.Map G.Vertex [(LocalTransitionId, G.Vertex)] ->
  G.Vertex ->
  LocalHintCompletionState ->
  LocalHintCompletionState
dfsCompleteLocal adjacency v state
  | v `Set.member` lhcSeen state = state
  | otherwise =
      let entered = state {lhcSeen = Set.insert v (lhcSeen state)}
          succs = Map.findWithDefault [] v adjacency
          afterSuccs = foldl' step entered succs
       in afterSuccs {lhcPathEntry = Map.delete v (lhcPathEntry afterSuccs)}
  where
    step acc (transitionId, succV)
      | succV `Map.member` lhcPathEntry acc =
          ensureEntryHintForAncestorLocal succV acc
      | succV `Set.member` lhcSeen acc = acc
      | otherwise =
          dfsCompleteLocal
            adjacency
            succV
            (acc {lhcPathEntry = Map.insert succV (Just transitionId) (lhcPathEntry acc)})

ensureEntryHintForAncestorLocal :: G.Vertex -> LocalHintCompletionState -> LocalHintCompletionState
ensureEntryHintForAncestorLocal ancestor state =
  case Map.lookup ancestor (lhcPathEntry state) of
    Nothing -> state
    Just Nothing ->
      if not (null (rvhBinders (lhcStartHints state)))
        then state
        else case rvhPreferredVar (lhcStartHints state) of
          Just tv ->
            state {lhcStartHints = addBinderHint tv (lhcStartHints state)}
          Nothing ->
            let (tv, state') = freshLocalHint state
             in state' {lhcStartHints = addBinderHint tv (lhcStartHints state')}
    Just (Just tid) ->
      let existing = Map.findWithDefault emptyRecVarHints tid (lhcTransitionHints state)
       in if not (null (rvhBinders existing))
            then state
            else case rvhPreferredVar existing of
              Just tv ->
                state
                  { lhcTransitionHints =
                      Map.insert tid (addBinderHint tv existing) (lhcTransitionHints state)
                  }
              Nothing ->
                let (tv, state') = freshLocalHint state
                 in state'
                      { lhcTransitionHints =
                          Map.insert tid (addBinderHint tv existing) (lhcTransitionHints state')
                      }

freshLocalHint :: LocalHintCompletionState -> (TypeVar, LocalHintCompletionState)
freshLocalHint state =
  let (tv, nextIx', used') = freshSynthetic (lhcUsedNames state) (lhcNextIx state)
   in ( tv
      , state
          { lhcUsedNames = used'
          , lhcNextIx = nextIx'
          }
      )

-- | Build a map from cyclic vertex -> assigned TypeVar for local graphs.
buildLocalCyclicVarMap ::
  G.Vertex ->
  RecVarHints ->
  Map.Map LocalTransitionId RecVarHints ->
  [(G.Vertex, G.Vertex, LocalTransitionId, RecVarHints)] ->
  Map.Map G.Vertex TypeVar
buildLocalCyclicVarMap startV startHints transHints transitions =
  let startEntry = case rvhBinders startHints of
        (tv : _) -> Map.singleton startV tv
        []       -> Map.empty
      transEntries = foldl' checkTrans Map.empty transitions
   in Map.union startEntry transEntries
  where
    checkTrans acc (_, to, tid, _) =
      case Map.lookup tid transHints of
        Just hints | (tv : _) <- rvhBinders hints ->
          Map.insertWith (\_ old -> old) to tv acc
        _ -> acc

-- | Propagate binders to edges targeting cyclic vertices, but only when the
-- edge can be a tree-edge in graphToType's all-paths traversal.
propagateLocalCyclicBinders ::
  G.Vertex ->
  [(G.Vertex, G.Vertex, LocalTransitionId, RecVarHints)] ->
  Map.Map G.Vertex TypeVar ->
  LocalHintCompletionState ->
  LocalHintCompletionState
propagateLocalCyclicBinders startV transitions cyclicVars state =
  foldl' propagate state transitions
  where
    simpleAdj = foldl' (\acc (from, to, _, _) -> Map.insertWith (++) from [to] acc) Map.empty transitions
    reachSets = Map.mapWithKey (\v _ -> reachableWithout simpleAdj startV v) cyclicVars

    propagate acc (from, to, tid, _)
      | from == to = acc  -- self-loops are always back-edges
      | otherwise =
          case Map.lookup to cyclicVars of
            Nothing -> acc
            Just tv ->
              case Map.lookup to reachSets of
                Nothing -> acc
                Just reachable | not (from `Set.member` reachable) -> acc
                Just _ ->
                  let existing = Map.findWithDefault emptyRecVarHints tid (lhcTransitionHints acc)
                   in if not (null (rvhBinders existing))
                        then acc
                        else acc { lhcTransitionHints = Map.insert tid (addBinderHint tv existing) (lhcTransitionHints acc) }

localTransitionId :: G.Vertex -> G.Vertex -> LocalEdgeLabel -> LocalTransitionId
localTransitionId from to lbl =
  (from, to, leDirection lbl, lePeer lbl, Left (leLabel lbl))

localPayloadTransitionId :: G.Vertex -> G.Vertex -> LocalPayloadEdgeLabel -> LocalTransitionId
localPayloadTransitionId from to lbl =
  (from, to, lpeDirection lbl, lpePeer lbl, Right (lpePayload lbl))

localTransitions :: LocalGraph -> [(G.Vertex, G.Vertex, LocalTransitionId, RecVarHints)]
localTransitions lg =
  [ (from, to, localTransitionId from to lbl, leTargetHints lbl)
  | ((from, to), labels) <- Map.toList (lgEdgeLabels lg)
  , lbl <- labels
  ]
  ++
  [ (from, to, localPayloadTransitionId from to lbl, lpeTargetHints lbl)
  | ((from, to), labels) <- Map.toList (lgPayloadEdges lg)
  , lbl <- labels
  ]

buildLocalAdjacency ::
  [(G.Vertex, G.Vertex, LocalTransitionId, RecVarHints)] ->
  Map.Map G.Vertex [(LocalTransitionId, G.Vertex)]
buildLocalAdjacency transitions =
  foldl' add Map.empty transitions
  where
    add acc (from, to, tid, _) =
      Map.insertWith (++) from [(tid, to)] acc

applyLocalTransitionHints ::
  LocalGraph ->
  Map.Map LocalTransitionId RecVarHints ->
  LocalGraph
applyLocalTransitionHints lg hintsByTransition =
  lg
    { lgEdgeLabels = Map.mapWithKey rewriteBranch (lgEdgeLabels lg)
    , lgPayloadEdges = Map.mapWithKey rewritePayload (lgPayloadEdges lg)
    }
  where
    rewriteBranch (from, to) labels =
      fmap (\lbl -> lbl {leTargetHints = Map.findWithDefault (leTargetHints lbl) (localTransitionId from to lbl) hintsByTransition}) labels
    rewritePayload (from, to) labels =
      fmap (\lbl -> lbl {lpeTargetHints = Map.findWithDefault (lpeTargetHints lbl) (localPayloadTransitionId from to lbl) hintsByTransition}) labels

allLocalHintNames ::
  RecVarHints ->
  [(G.Vertex, G.Vertex, LocalTransitionId, RecVarHints)] ->
  Set.Set String
allLocalHintNames startHints transitions =
  Set.fromList
    ( fmap getTypeVar (recVarHintsToList startHints)
        ++ [ getTypeVar tv
           | (_, _, _, hints) <- transitions
           , tv <- recVarHintsToList hints
           ]
    )

localOutgoing :: LocalGraph -> Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)]
localOutgoing lg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty
    (lgEdgeLabels lg)

localPayloadOutgoing :: LocalGraph -> Map.Map G.Vertex [(LocalPayloadEdgeLabel, G.Vertex)]
localPayloadOutgoing lg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty
    (lgPayloadEdges lg)

localNode :: Env.Map TypeVar G.Vertex -> LocalType -> State (GraphBuilder LocalNode (Either LocalEdgeLabel LocalPayloadEdgeLabel)) G.Vertex
localNode env lt = case lt of
  LSend peer branches -> do
    let labels = fmap fst (NE.toList branches)
    v <- freshNode (LocalSendNode peer labels)
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- localNode env cont
      addEdge v dest (Left (LocalEdgeLabel Send peer lbl (entryHintsLocal cont)))
    pure v
  LRecv peer branches -> do
    let labels = fmap fst (NE.toList branches)
    v <- freshNode (LocalRecvNode peer labels)
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- localNode env cont
      addEdge v dest (Left (LocalEdgeLabel Receive peer lbl (entryHintsLocal cont)))
    pure v
  LPayloadSend peer pt cont -> do
    v <- freshNode (LocalPayloadSendNode peer pt)
    dest <- localNode env cont
    addEdge v dest (Right (LocalPayloadEdgeLabel Send peer pt (entryHintsLocal cont)))
    pure v
  LPayloadRecv peer pt cont -> do
    v <- freshNode (LocalPayloadRecvNode peer pt)
    dest <- localNode env cont
    addEdge v dest (Right (LocalPayloadEdgeLabel Receive peer pt (entryHintsLocal cont)))
    pure v
  LVar var -> lookupVar env var
  LRec var body ->
    mfix $ \start -> localNode (Env.insert var start env) body
  LEnd -> freshNode LocalEndNode

finaliseLocal :: G.Vertex -> RecVarHints -> GraphBuilder LocalNode (Either LocalEdgeLabel LocalPayloadEdgeLabel) -> LocalGraph
finaliseLocal start startHints builder =
  let bounds = graphBounds builder
      allEdges = gbEdges builder
      graph = G.buildG bounds (map fst allEdges)
      nodeTable = array bounds (Map.toList (gbNodes builder))
      branchEdges = [(e, lbl) | (e, Left lbl) <- allEdges]
      payloadEdges = [(e, plbl) | (e, Right plbl) <- allEdges]
   in LocalGraph
        { lgGraph = graph
        , lgStart = start
        , lgNodes = nodeTable
        , lgEdgeLabels = collectEdges branchEdges
        , lgPayloadEdges = collectEdges payloadEdges
        , lgStartVarHints = normaliseRecVarHints startHints
        }

entryHintsLocal :: LocalType -> RecVarHints
entryHintsLocal =
  go []
  where
    go binders ltype =
      case ltype of
        LRec var body -> go (var : binders) body
        LVar var ->
          normaliseRecVarHints
            ( RecVarHints
                { rvhBinders = reverse binders
                , rvhPreferredVar = Just var
                }
            )
        _ ->
          normaliseRecVarHints
            ( RecVarHints
                { rvhBinders = reverse binders
                , rvhPreferredVar = Nothing
                }
            )

-- Context graphs

-- | Context automaton state represented as a map from participant to local vertex.
newtype ContextState = ContextState { unContextState :: Map.Map Participant G.Vertex }
  deriving (Eq, Ord, Show, Generic)

instance NFData ContextState

-- | Edge labels in a context automaton.
data ContextEdgeLabel
  = ContextSingleEdge
      { ceActor :: Participant
      , ceDirection :: LocalDirection
      , cePeer :: Participant
      , ceLabel :: Label
      }
  | ContextSyncEdge
      { ceSender :: Participant
      , ceReceiver :: Participant
      , ceLabel :: Label
      }
  | ContextPayloadSingleEdge
      Participant     -- actor
      LocalDirection  -- direction
      Participant     -- peer
      PayloadType     -- payload type
  | ContextPayloadSyncEdge
      Participant     -- sender
      Participant     -- receiver
      PayloadType     -- payload type
  deriving (Eq, Ord, Show, Generic)

instance NFData ContextEdgeLabel

-- | Product/context automaton over a list of local automata.
--
-- Invariant:
--
-- For each source state and triple @(sender, receiver, label)@, a
-- 'ContextSyncEdge' exists if and only if both corresponding single-sided
-- edges exist:
--
-- * sender has a 'ContextSingleEdge' with 'Send' to receiver with that label
-- * receiver has a 'ContextSingleEdge' with 'Receive' from sender with that label
--
-- See 'checkContextSynchrony'.
data ContextGraph = ContextGraph
  { cgGraph :: G.Graph
  , cgStart :: G.Vertex
  , cgNodes :: G.Table ContextState
  , cgParticipants :: [Participant]
  , cgEdgeLabels :: Map.Map G.Edge [ContextEdgeLabel]
  }
  deriving (Eq, Show, Generic)

instance NFData ContextGraph

-- | Build a context automaton from participant-indexed local automata.
--
-- States are participant-indexed local vertices, and edges include:
--
-- * single-sided transitions, where one component moves
-- * synchronized transitions, where exactly one send and one compatible
--   receive move together
--
-- Postcondition: the resulting graph satisfies 'checkContextSynchrony'.
buildContextGraph :: [(Participant, LocalGraph)] -> ContextGraph
buildContextGraph automata
  | null automata = error "Automata: cannot build context graph from empty input"
  | Map.size components /= length participants =
      error "Automata: duplicate participants in context automaton input"
  | otherwise =
      let graph = finaliseContext participants (exploreContext components startState)
       in case checkContextSynchrony graph of
            Right () -> graph
            Left errs ->
              error
                ( "Automata: internal context synchrony invariant violated: "
                    ++ show errs
                )
  where
    participants = fmap fst automata
    components = Map.fromList [(p, mkComponent p lg) | (p, lg) <- automata]
    startState =
      ContextState (Map.fromList [(p, cStart c) | (p, c) <- Map.toList components])

data ContextBuilder = ContextBuilder
  { cbNext :: !G.Vertex
  , cbSeen :: Map.Map ContextState G.Vertex
  , cbNodes :: Map.Map G.Vertex ContextState
  , cbEdges :: [(G.Edge, ContextEdgeLabel)]
  , cbQueue :: Seq.Seq ContextState
  }

data LocalStep
  = BranchStep !G.Vertex LocalEdgeLabel
  | PayloadStep !G.Vertex LocalPayloadEdgeLabel

stepTarget :: LocalStep -> G.Vertex
stepTarget (BranchStep v _) = v
stepTarget (PayloadStep v _) = v

-- | Violations of the context synchrony invariant.
data ContextInvariantError
  = MissingSyncForSingles G.Vertex Participant Participant Label
  | SyncWithoutSingles G.Vertex Participant Participant Label
  | MissingSyncForPayloadSingles G.Vertex Participant Participant PayloadType
  | PayloadSyncWithoutSingles G.Vertex Participant Participant PayloadType
  deriving (Eq, Ord, Show, Generic)

instance NFData ContextInvariantError

data Component = Component
  { cStart :: !G.Vertex
  , cOutgoing :: Map.Map G.Vertex [LocalStep]
  }

mkComponent :: Participant -> LocalGraph -> Component
mkComponent _ lg =
  Component
    { cStart = lgStart lg
    , cOutgoing = outgoingSteps lg
    }

outgoingSteps :: LocalGraph -> Map.Map G.Vertex [LocalStep]
outgoingSteps lg =
  foldl' addBranch (foldl' addPayload Map.empty payloadPairs) branchPairs
  where
    branchPairs = Map.toList (lgEdgeLabels lg)
    payloadPairs = Map.toList (lgPayloadEdges lg)
    addBranch acc ((from, to), labels) =
      foldl' (\m lbl -> Map.insertWith (++) from [BranchStep to lbl] m) acc labels
    addPayload acc ((from, to), labels) =
      foldl' (\m lbl -> Map.insertWith (++) from [PayloadStep to lbl] m) acc labels

exploreContext :: Map.Map Participant Component -> ContextState -> ContextBuilder
exploreContext components startState = go initial
  where
    initial =
      ContextBuilder
        { cbNext = 1
        , cbSeen = Map.singleton startState 0
        , cbNodes = Map.singleton 0 startState
        , cbEdges = []
        , cbQueue = Seq.singleton startState
        }

    go builder =
      case Seq.viewl (cbQueue builder) of
        Seq.EmptyL -> builder
        state Seq.:< rest ->
          let from = cbSeen builder Map.! state
              builder' =
                foldl'
                  (addTransition from)
                  (builder {cbQueue = rest})
                  (contextTransitions components state)
           in go builder'

addTransition :: G.Vertex -> ContextBuilder -> (ContextState, ContextEdgeLabel) -> ContextBuilder
addTransition from builder (state, label) =
  let (to, builder') = ensureContextState state builder
   in builder' {cbEdges = ((from, to), label) : cbEdges builder'}

ensureContextState :: ContextState -> ContextBuilder -> (G.Vertex, ContextBuilder)
ensureContextState state builder =
  case Map.lookup state (cbSeen builder) of
    Just v -> (v, builder)
    Nothing ->
      let v = cbNext builder
       in ( v
          , builder
              { cbNext = v + 1
              , cbSeen = Map.insert state v (cbSeen builder)
              , cbNodes = Map.insert v state (cbNodes builder)
              , cbQueue = cbQueue builder Seq.|> state
              }
          )

contextTransitions :: Map.Map Participant Component -> ContextState -> [(ContextState, ContextEdgeLabel)]
contextTransitions components (ContextState states) =
  singleTransitions ++ syncTransitions
  where
    stateAt :: Participant -> G.Vertex
    stateAt participant =
      case Map.lookup participant states of
        Just v -> v
        Nothing ->
          error ("Automata: context state missing participant " ++ show participant)

    outgoingAt :: Participant -> Component -> [LocalStep]
    outgoingAt participant component =
      Map.findWithDefault [] (stateAt participant) (cOutgoing component)

    singleTransitions =
      [ case step of
          BranchStep to lbl ->
            ( ContextState (Map.insert participant to states)
            , ContextSingleEdge
                { ceActor = participant
                , ceDirection = leDirection lbl
                , cePeer = lePeer lbl
                , ceLabel = leLabel lbl
                }
            )
          PayloadStep to lbl ->
            ( ContextState (Map.insert participant to states)
            , ContextPayloadSingleEdge participant (lpeDirection lbl) (lpePeer lbl) (lpePayload lbl)
            )
      | (participant, comp) <- Map.toList components
      , step <- outgoingAt participant comp
      ]

    syncTransitions = branchSyncs ++ payloadSyncs

    branchSyncs =
      [ ( ContextState
            (Map.insert receiver (stepTarget recvStep) (Map.insert sender (stepTarget sendStep) states))
        , ContextSyncEdge
            { ceSender = sender
            , ceReceiver = receiver
            , ceLabel = leLabel sendLbl
            }
        )
      | (sender, senderComp) <- Map.toList components
      , sendStep@(BranchStep _ sendLbl) <- outgoingAt sender senderComp
      , leDirection sendLbl == Send
      , let receiver = lePeer sendLbl
      , Just receiverComp <- [Map.lookup receiver components]
      , recvStep@(BranchStep _ recvLbl) <- outgoingAt receiver receiverComp
      , leDirection recvLbl == Receive
      , lePeer recvLbl == sender
      , leLabel recvLbl == leLabel sendLbl
      ]

    payloadSyncs =
      [ ( ContextState
            (Map.insert receiver (stepTarget recvStep) (Map.insert sender (stepTarget sendStep) states))
        , ContextPayloadSyncEdge sender receiver (lpePayload sendLbl)
        )
      | (sender, senderComp) <- Map.toList components
      , sendStep@(PayloadStep _ sendLbl) <- outgoingAt sender senderComp
      , lpeDirection sendLbl == Send
      , let receiver = lpePeer sendLbl
      , Just receiverComp <- [Map.lookup receiver components]
      , recvStep@(PayloadStep _ recvLbl) <- outgoingAt receiver receiverComp
      , lpeDirection recvLbl == Receive
      , lpePeer recvLbl == sender
      , lpePayload recvLbl == lpePayload sendLbl
      ]

finaliseContext :: [Participant] -> ContextBuilder -> ContextGraph
finaliseContext participants builder =
  let bounds = contextBounds builder
      graph = G.buildG bounds (map fst (cbEdges builder))
      nodeTable = array bounds (Map.toList (cbNodes builder))
      edgeLabels = collectEdges (cbEdges builder)
   in ContextGraph
        { cgGraph = graph
        , cgStart = 0
        , cgNodes = nodeTable
        , cgParticipants = participants
        , cgEdgeLabels = edgeLabels
        }

-- | Check that sync edges exist exactly when both compatible singles exist.
checkContextSynchrony :: ContextGraph -> Either [ContextInvariantError] ()
checkContextSynchrony cg =
  case concatMap checkSource sources of
    [] -> Right ()
    errs -> Left errs
  where
    sources = fmap fst (assocs (cgNodes cg))
    outgoingBySource = collectOutgoing (cgEdgeLabels cg)

    checkSource :: G.Vertex -> [ContextInvariantError]
    checkSource source =
      let outgoing = Map.findWithDefault [] source outgoingBySource
          -- Branch synchrony
          sendKeys = Set.fromList [k | (_, ContextSingleEdge a Send p l) <- outgoing, let k = (a, p, l)]
          recvKeys = Set.fromList [k | (_, ContextSingleEdge a Receive p l) <- outgoing, let k = (p, a, l)]
          syncKeys = Set.fromList [k | (_, ContextSyncEdge s r l) <- outgoing, let k = (s, r, l)]
          requiredSync = Set.intersection sendKeys recvKeys
          missingSync = Set.difference requiredSync syncKeys
          spuriousSync = Set.difference syncKeys requiredSync
          -- Payload synchrony
          pSendKeys = Set.fromList [k | (_, ContextPayloadSingleEdge a Send p pt) <- outgoing, let k = (a, p, pt)]
          pRecvKeys = Set.fromList [k | (_, ContextPayloadSingleEdge a Receive p pt) <- outgoing, let k = (p, a, pt)]
          pSyncKeys = Set.fromList [k | (_, ContextPayloadSyncEdge s r pt) <- outgoing, let k = (s, r, pt)]
          pRequiredSync = Set.intersection pSendKeys pRecvKeys
          pMissingSync = Set.difference pRequiredSync pSyncKeys
          pSpuriousSync = Set.difference pSyncKeys pRequiredSync
          -- Errors
          missingSyncErrs = [MissingSyncForSingles source s r l | (s, r, l) <- Set.toList missingSync]
          spuriousSyncErrs = [SyncWithoutSingles source s r l | (s, r, l) <- Set.toList spuriousSync]
          pMissingSyncErrs = [MissingSyncForPayloadSingles source s r pt | (s, r, pt) <- Set.toList pMissingSync]
          pSpuriousSyncErrs = [PayloadSyncWithoutSingles source s r pt | (s, r, pt) <- Set.toList pSpuriousSync]
       in missingSyncErrs ++ spuriousSyncErrs ++ pMissingSyncErrs ++ pSpuriousSyncErrs

collectOutgoing :: Map.Map G.Edge [ContextEdgeLabel] -> Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)]
collectOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(to, lbl)] m) acc labels)
    Map.empty

contextBounds :: ContextBuilder -> (G.Vertex, G.Vertex)
contextBounds builder
  | cbNext builder <= 0 = error "Automata: no context vertices generated"
  | otherwise = (0, cbNext builder - 1)

collectEdges :: Ord k => [(k, v)] -> Map.Map k [v]
collectEdges = foldr step Map.empty
  where
    step (k, v) acc = Map.insertWith (++) k [v] acc
