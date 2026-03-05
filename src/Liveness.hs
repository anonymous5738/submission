-- | Liveness utilities and checks for context graphs.
module Liveness
  ( SyncPath(..)
  , PathLivenessViolation(..)
  , LivenessError(..)
  , LivenessResult
  , allSyncPaths
  , isFairSyncPath
  , filterFairSyncPaths
  , fairSyncPaths
  , isLiveSyncPath
  , filterLiveSyncPaths
  , liveFairSyncPaths
  , checkLiveness
  ) where

import Automata (ContextEdgeLabel(..), ContextGraph(..), LocalDirection(..))
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Participant)

-- | Sync-only paths used by liveness checking.
--
-- 'FiniteSyncPath' stores a finite simple path that ends in a sync-terminal
-- state.
--
-- 'LassoSyncPath' stores an infinite lasso path as a finite stem plus a
-- non-empty cycle repeated forever.
data SyncPath
  = FiniteSyncPath (NE.NonEmpty G.Vertex)
  | LassoSyncPath [G.Vertex] (NE.NonEmpty G.Vertex)
  deriving (Eq, Ord, Show)

-- | Violation of path-level liveness at a concrete state/action.
data PathLivenessViolation = MissingOppositeAction
  { plState :: !G.Vertex
  , plActor :: Participant
  , plDirection :: LocalDirection
  , plPeer :: Participant
  }
  deriving (Eq, Ord, Show)

-- | Liveness violations over fair paths.
data LivenessError = FairPathNotLive
  { lePath :: SyncPath
  , leViolations :: [PathLivenessViolation]
  }
  deriving (Eq, Show)

-- | Result of liveness checking.
type LivenessResult = Either [LivenessError] ()

-- | Enumerate all sync-only paths from 'cgStart'.
--
-- The result contains:
--
-- * finite simple paths ending at states with no outgoing sync transitions
-- * infinite lasso paths (simple stem + simple cycle)
allSyncPaths :: ContextGraph -> [SyncPath]
allSyncPaths cg =
  Set.toList (Set.fromList (go [start]))
  where
    start = cgStart cg
    adjacency = syncAdjacency cg

    go :: [G.Vertex] -> [SyncPath]
    go path =
      finiteHere ++ lassoHere ++ concatMap (\succV -> go (path ++ [succV])) forwardSuccs
      where
        current = last path
        succs = Map.findWithDefault [] current adjacency
        finiteHere =
          if null succs
            then [FiniteSyncPath (toNonEmpty path)]
            else []
        lassoHere =
          [ mkLasso path succV
          | succV <- succs
          , succV `elem` path
          ]
        forwardSuccs =
          [ succV
          | succV <- succs
          , succV `notElem` path
          ]

toNonEmpty :: [a] -> NE.NonEmpty a
toNonEmpty [] = error "Liveness: expected non-empty path."
toNonEmpty (x : xs) = x NE.:| xs

mkLasso :: [G.Vertex] -> G.Vertex -> SyncPath
mkLasso path loopStart =
  case elemIndex loopStart path of
    Nothing ->
      error
        ( "Liveness: cannot form lasso; loop start "
            ++ show loopStart
            ++ " not present in current path."
        )
    Just ix ->
      let stem = take ix path
          cycleStates = drop ix path
       in LassoSyncPath stem (toNonEmpty cycleStates)

syncAdjacency :: ContextGraph -> Map.Map G.Vertex [G.Vertex]
syncAdjacency cg =
  Map.map Set.toList $
    Map.foldlWithKey' addSyncEdge Map.empty (cgEdgeLabels cg)
  where
    addSyncEdge acc (from, to) labels
      | any isSync labels = Map.insertWith Set.union from (Set.singleton to) acc
      | otherwise = acc

    isSync ContextSyncEdge{} = True
    isSync _ = False

type ParticipantPair = (Participant, Participant)

-- | Check whether a sync path is fair.
--
-- A path is fair iff for every state occurrence on the path, every enabled
-- sync participant-pair at that state appears eventually on some subsequent
-- path step.
isFairSyncPath :: ContextGraph -> SyncPath -> Bool
isFairSyncPath cg path =
  case path of
    FiniteSyncPath states ->
      checkFinite (NE.toList states)
    LassoSyncPath stem cycleStates ->
      checkLasso stem (NE.toList cycleStates)
  where
    enabledAt = enabledPairsByState cg
    stepPairs from to = syncPairsBetween cg from to

    checkFinite states =
      let edgePairsByStep = fmap (uncurry stepPairs) (stateEdges states)
          suffixPairs = suffixUnions edgePairsByStep
       in and
            [ Map.findWithDefault Set.empty st enabledAt
                `Set.isSubsetOf` future
            | (st, future) <- zip states suffixPairs
            ]

    checkLasso stem cycleStates =
      let stemStepPairs = fmap (uncurry stepPairs) (stateEdgesWithEntry stem cycleStates)
          cycleStepPairs = fmap (uncurry stepPairs) (cycleEdges cycleStates)
          cyclePairs = Set.unions cycleStepPairs
          stemSuffixPairs = fmap (`Set.union` cyclePairs) (suffixUnions stemStepPairs)
          stemOk =
            and
              [ Map.findWithDefault Set.empty st enabledAt
                  `Set.isSubsetOf` future
              | (st, future) <- zip stem stemSuffixPairs
              ]
          cycleOk =
            and
              [ Map.findWithDefault Set.empty st enabledAt
                  `Set.isSubsetOf` cyclePairs
              | st <- cycleStates
              ]
       in stemOk && cycleOk

-- | Filter a list of sync paths to only fair paths.
filterFairSyncPaths :: ContextGraph -> [SyncPath] -> [SyncPath]
filterFairSyncPaths cg =
  filter (isFairSyncPath cg)

-- | Enumerate all fair sync-only paths from a context graph.
fairSyncPaths :: ContextGraph -> [SyncPath]
fairSyncPaths cg =
  filterFairSyncPaths cg (allSyncPaths cg)

enabledPairsByState :: ContextGraph -> Map.Map G.Vertex (Set.Set ParticipantPair)
enabledPairsByState cg =
  Map.fromSet enabledAtVertex allVertices
  where
    allVertices = Set.fromList (G.vertices (cgGraph cg))
    enabledAtVertex v =
      Set.fromList
        [ (sender, receiver)
        | ((from, _), labels) <- Map.toList (cgEdgeLabels cg)
        , from == v
        , ContextSyncEdge sender receiver _ <- labels
        ]

syncPairsBetween :: ContextGraph -> G.Vertex -> G.Vertex -> Set.Set ParticipantPair
syncPairsBetween cg from to =
  Set.fromList
    [ (sender, receiver)
    | ContextSyncEdge sender receiver _ <- Map.findWithDefault [] (from, to) (cgEdgeLabels cg)
    ]

suffixUnions :: Ord a => [Set.Set a] -> [Set.Set a]
suffixUnions xs =
  scanr Set.union Set.empty xs

stateEdges :: [G.Vertex] -> [(G.Vertex, G.Vertex)]
stateEdges states =
  zip states (drop 1 states)

stateEdgesWithEntry :: [G.Vertex] -> [G.Vertex] -> [(G.Vertex, G.Vertex)]
stateEdgesWithEntry stem cycleStates =
  case stem of
    [] -> []
    _ -> zip stem (drop 1 stem ++ [head cycleStates])

cycleEdges :: [G.Vertex] -> [(G.Vertex, G.Vertex)]
cycleEdges cycleStates =
  zip cycleStates (tail cycleStates ++ [head cycleStates])

type PendingAction = (Participant, LocalDirection, Participant)

-- | Check whether a sync path is live.
--
-- A path is live iff every pending single-sided action eventually encounters
-- the corresponding opposite willingness (labels may differ).
isLiveSyncPath :: ContextGraph -> SyncPath -> Bool
isLiveSyncPath cg path =
  null (pathLivenessViolations cg path)

-- | Filter a list of sync paths to only live paths.
filterLiveSyncPaths :: ContextGraph -> [SyncPath] -> [SyncPath]
filterLiveSyncPaths cg =
  filter (isLiveSyncPath cg)

-- | Enumerate fair sync paths that are also live.
liveFairSyncPaths :: ContextGraph -> [SyncPath]
liveFairSyncPaths cg =
  filterLiveSyncPaths cg (fairSyncPaths cg)

pathLivenessViolations :: ContextGraph -> SyncPath -> [PathLivenessViolation]
pathLivenessViolations cg path =
  Set.toList $
    case path of
      FiniteSyncPath states ->
        finiteViolations (NE.toList states)
      LassoSyncPath stem cycleStates ->
        lassoViolations stem (NE.toList cycleStates)
  where
    enabledSingles = enabledSingleActionsByState cg

    finiteViolations states =
      let enabledByState = fmap (\st -> Map.findWithDefault Set.empty st enabledSingles) states
          futureSingles = suffixUnions enabledByState
       in Set.fromList
            [ MissingOppositeAction st actor direction peer
            | (st, pending, future) <- zip3 states enabledByState futureSingles
            , (actor, direction, peer) <- Set.toList pending
            , oppositeAction (actor, direction, peer) `Set.notMember` future
            ]

    lassoViolations stem cycleStates =
      let stemEnabled = fmap (\st -> Map.findWithDefault Set.empty st enabledSingles) stem
          cycleEnabled = fmap (\st -> Map.findWithDefault Set.empty st enabledSingles) cycleStates
          cycleUnion = Set.unions cycleEnabled
          stemFuture = fmap (`Set.union` cycleUnion) (suffixUnions stemEnabled)
          stemMissing =
            [ MissingOppositeAction st actor direction peer
            | (st, pending, future) <- zip3 stem stemEnabled stemFuture
            , (actor, direction, peer) <- Set.toList pending
            , oppositeAction (actor, direction, peer) `Set.notMember` future
            ]
          cycleMissing =
            [ MissingOppositeAction st actor direction peer
            | (st, pending) <- zip cycleStates cycleEnabled
            , (actor, direction, peer) <- Set.toList pending
            , oppositeAction (actor, direction, peer) `Set.notMember` cycleUnion
            ]
       in Set.fromList (stemMissing ++ cycleMissing)

oppositeAction :: PendingAction -> PendingAction
oppositeAction (actor, direction, peer) =
  case direction of
    Send -> (peer, Receive, actor)
    Receive -> (peer, Send, actor)

enabledSingleActionsByState :: ContextGraph -> Map.Map G.Vertex (Set.Set PendingAction)
enabledSingleActionsByState cg =
  Map.fromSet enabledAtVertex allVertices
  where
    allVertices = Set.fromList (G.vertices (cgGraph cg))
    enabledAtVertex v =
      Set.fromList
        [ (actor, direction, peer)
        | ((from, _), labels) <- Map.toList (cgEdgeLabels cg)
        , from == v
        , ContextSingleEdge actor direction peer _ <- labels
        ]

-- | Check whether a context graph satisfies liveness/progress requirements.
checkLiveness :: ContextGraph -> LivenessResult
checkLiveness cg =
  case violations of
    [] -> Right ()
    errs -> Left errs
  where
    violations =
      [ FairPathNotLive path pathViolations
      | path <- fairSyncPaths cg
      , let pathViolations = pathLivenessViolations cg path
      , not (null pathViolations)
      ]
