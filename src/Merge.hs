-- | Merge operators for local graphs used by projection.
module Merge
  ( Merge
  , iso
  , plainMerge
  , fullMerge
  ) where

import Automata
  ( LocalEdgeLabel(..)
  , LocalDirection(..)
  , LocalGraph(..)
  , LocalNode(..)
  , LocalPayloadEdgeLabel(..)
  , RecVarHints(..)
  )
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State.Strict (StateT(..), gets, modify, runStateT)
import Data.Array (array, assocs)
import Data.Foldable (foldl')
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Label, Participant, TypeVar)

-- | Merge strategy for two local graphs.
type Merge = LocalGraph -> LocalGraph -> Maybe LocalGraph

-- | Plain merge: succeeds iff graphs are isomorphic; returns the left graph.
plainMerge :: Merge
plainMerge left right =
  if iso left right
    then Just left
    else Nothing

-- | Full merge for local graphs.
--
-- Rules:
--
-- * send nodes must have identical label sets;
-- * receive nodes may have different label sets and are unioned;
-- * common labels recurse by merging targets;
-- * one-sided receive-only labels are preserved as one-sided subgraphs.
fullMerge :: Merge
fullMerge left right = do
  leftOut <- outgoingByLabelMap (lgEdgeLabels left)
  rightOut <- outgoingByLabelMap (lgEdgeLabels right)
  let input =
        MergeInput
          { miLeftNodes = Map.fromList (assocs (lgNodes left))
          , miRightNodes = Map.fromList (assocs (lgNodes right))
          , miLeftOut = leftOut
          , miRightOut = rightOut
          , miLeftPayloadOut = payloadOutgoingByVertex (lgPayloadEdges left)
          , miRightPayloadOut = payloadOutgoingByVertex (lgPayloadEdges right)
          }
      start = Align (Just (lgStart left)) (Just (lgStart right))
      startHints = mergeHints (lgStartVarHints left) (lgStartVarHints right)
      initState =
        MergeBuild
          { mbNext = 0
          , mbAlignToOut = Map.empty
          , mbExpanded = Set.empty
          , mbFwd = Map.empty
          , mbBwd = Map.empty
          , mbNodes = Map.empty
          , mbEdges = Set.empty
          , mbPayloadEdges = Set.empty
          }
  (startOut, st) <- runStateT (visit input start) initState
  buildMergedGraph startHints startOut st

-- | Rooted local-graph isomorphism with a forward/backward partial bijection.
iso :: LocalGraph -> LocalGraph -> Bool
iso left right = go Map.empty Map.empty [(lgStart left, lgStart right)]
  where
    leftNodes = Map.fromList (assocs (lgNodes left))
    rightNodes = Map.fromList (assocs (lgNodes right))
    leftOut = outgoingBySource (lgEdgeLabels left)
    rightOut = outgoingBySource (lgEdgeLabels right)
    leftPayloadOut = payloadOutgoingBySource (lgPayloadEdges left)
    rightPayloadOut = payloadOutgoingBySource (lgPayloadEdges right)

    go :: Map.Map G.Vertex G.Vertex -> Map.Map G.Vertex G.Vertex -> [(G.Vertex, G.Vertex)] -> Bool
    go _ _ [] = True
    go fwd bwd ((x, y) : pending) =
      case (Map.lookup x fwd, Map.lookup y bwd) of
        (Just y', Just x') -> y' == y && x' == x && go fwd bwd pending
        (Nothing, Nothing) ->
          case (Map.lookup x leftNodes, Map.lookup y rightNodes) of
            (Just nx, Just ny) ->
              nodeCompatible nx ny
                && let fwd' = Map.insert x y fwd
                       bwd' = Map.insert y x bwd
                   in case (nx, ny) of
                        (LocalPayloadSendNode{}, _) ->
                          case (Map.lookup x leftPayloadOut, Map.lookup y rightPayloadOut) of
                            (Just (_, lDst), Just (_, rDst)) -> go fwd' bwd' ((lDst, rDst) : pending)
                            _ -> False
                        (LocalPayloadRecvNode{}, _) ->
                          case (Map.lookup x leftPayloadOut, Map.lookup y rightPayloadOut) of
                            (Just (_, lDst), Just (_, rDst)) -> go fwd' bwd' ((lDst, rDst) : pending)
                            _ -> False
                        _ ->
                          case (successorsByLabel leftOut x, successorsByLabel rightOut y) of
                            (Just sx, Just sy) ->
                              Map.keysSet sx == Map.keysSet sy
                                && let next = [(sx Map.! l, sy Map.! l) | l <- Map.keys sx]
                                   in go fwd' bwd' (next ++ pending)
                            _ -> False
            _ -> False
        _ -> False

data Align = Align
  { alLeft :: Maybe G.Vertex
  , alRight :: Maybe G.Vertex
  }
  deriving (Eq, Ord, Show)

data MergeInput = MergeInput
  { miLeftNodes :: Map.Map G.Vertex LocalNode
  , miRightNodes :: Map.Map G.Vertex LocalNode
  , miLeftOut :: Map.Map G.Vertex (Map.Map Label (LocalEdgeLabel, G.Vertex))
  , miRightOut :: Map.Map G.Vertex (Map.Map Label (LocalEdgeLabel, G.Vertex))
  , miLeftPayloadOut :: Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex)
  , miRightPayloadOut :: Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex)
  }

data MergeBuild = MergeBuild
  { mbNext :: !G.Vertex
  , mbAlignToOut :: Map.Map Align G.Vertex
  , mbExpanded :: Set.Set Align
  , mbFwd :: Map.Map G.Vertex (Maybe G.Vertex)
  , mbBwd :: Map.Map G.Vertex (Maybe G.Vertex)
  , mbNodes :: Map.Map G.Vertex LocalNode
  , mbEdges :: Set.Set (G.Edge, LocalEdgeLabel)
  , mbPayloadEdges :: Set.Set (G.Edge, LocalPayloadEdgeLabel)
  }

data TransitionPlan
  = TransitionPlan LocalEdgeLabel Align
  | PayloadTransitionPlan LocalPayloadEdgeLabel Align

type MergeM a = StateT MergeBuild Maybe a

visit :: MergeInput -> Align -> MergeM G.Vertex
visit input align = do
  enforceAlignment align
  outV <- ensureOutVertex align
  isExpanded <- gets (Set.member align . mbExpanded)
  if isExpanded
    then pure outV
    else do
      modify (\s -> s {mbExpanded = Set.insert align (mbExpanded s)})
      (node, plans) <- hoistMaybe (expandAlign input align)
      putNode outV node
      mapM_ (addTransition input outV) plans
      pure outV

addTransition :: MergeInput -> G.Vertex -> TransitionPlan -> MergeM ()
addTransition input from (TransitionPlan edgeLbl nextAlign) = do
  to <- visit input nextAlign
  modify (\s -> s {mbEdges = Set.insert ((from, to), edgeLbl) (mbEdges s)})
addTransition input from (PayloadTransitionPlan edgeLbl nextAlign) = do
  to <- visit input nextAlign
  modify (\s -> s {mbPayloadEdges = Set.insert ((from, to), edgeLbl) (mbPayloadEdges s)})

ensureOutVertex :: Align -> MergeM G.Vertex
ensureOutVertex align = do
  existing <- gets (Map.lookup align . mbAlignToOut)
  case existing of
    Just v -> pure v
    Nothing -> do
      v <- gets mbNext
      modify
        ( \s ->
            s
              { mbNext = v + 1
              , mbAlignToOut = Map.insert align v (mbAlignToOut s)
              }
        )
      pure v

putNode :: G.Vertex -> LocalNode -> MergeM ()
putNode v node = do
  existing <- gets (Map.lookup v . mbNodes)
  case existing of
    Nothing -> modify (\s -> s {mbNodes = Map.insert v node (mbNodes s)})
    Just node' ->
      guard (node' == node)

enforceAlignment :: Align -> MergeM ()
enforceAlignment (Align left right) =
  case (left, right) of
    (Nothing, Nothing) -> hoistMaybe Nothing
    (Just x, Just y) -> bindFwd x (Just y) >> bindBwd y (Just x)
    (Just x, Nothing) -> bindFwd x Nothing
    (Nothing, Just y) -> bindBwd y Nothing

bindFwd :: G.Vertex -> Maybe G.Vertex -> MergeM ()
bindFwd x y = do
  existing <- gets (Map.lookup x . mbFwd)
  case existing of
    Nothing -> modify (\s -> s {mbFwd = Map.insert x y (mbFwd s)})
    Just y' -> guard (y' == y)

bindBwd :: G.Vertex -> Maybe G.Vertex -> MergeM ()
bindBwd y x = do
  existing <- gets (Map.lookup y . mbBwd)
  case existing of
    Nothing -> modify (\s -> s {mbBwd = Map.insert y x (mbBwd s)})
    Just x' -> guard (x' == x)

expandAlign :: MergeInput -> Align -> Maybe (LocalNode, [TransitionPlan])
expandAlign input (Align left right) =
  case (left, right) of
    (Just x, Just y) -> do
      nx <- Map.lookup x (miLeftNodes input)
      ny <- Map.lookup y (miRightNodes input)
      sx <- pure (outAt (miLeftOut input) x)
      sy <- pure (outAt (miRightOut input) y)
      case (nx, ny) of
        (LocalPayloadSendNode lp lpt, LocalPayloadSendNode rp rpt)
          | lp == rp && lpt == rpt -> mergePayloadBoth input nx x y
        (LocalPayloadRecvNode lp lpt, LocalPayloadRecvNode rp rpt)
          | lp == rp && lpt == rpt -> mergePayloadBoth input nx x y
        _ -> do
          validateOutgoing nx sx
          validateOutgoing ny sy
          mergeBoth nx ny sx sy
    (Just x, Nothing) -> do
      nx <- Map.lookup x (miLeftNodes input)
      sx <- pure (outAt (miLeftOut input) x)
      case nx of
        LocalPayloadSendNode _ _ -> clonePayload (miLeftPayloadOut input) nx x (\dst -> Align (Just dst) Nothing)
        LocalPayloadRecvNode _ _ -> clonePayload (miLeftPayloadOut input) nx x (\dst -> Align (Just dst) Nothing)
        _ -> do
          validateOutgoing nx sx
          cloneLeft nx sx
    (Nothing, Just y) -> do
      ny <- Map.lookup y (miRightNodes input)
      sy <- pure (outAt (miRightOut input) y)
      case ny of
        LocalPayloadSendNode _ _ -> clonePayload (miRightPayloadOut input) ny y (\dst -> Align Nothing (Just dst))
        LocalPayloadRecvNode _ _ -> clonePayload (miRightPayloadOut input) ny y (\dst -> Align Nothing (Just dst))
        _ -> do
          validateOutgoing ny sy
          cloneRight ny sy
    (Nothing, Nothing) -> Nothing

mergeBoth ::
  LocalNode ->
  LocalNode ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Maybe (LocalNode, [TransitionPlan])
mergeBoth leftNode rightNode sx sy =
  case (leftNode, rightNode) of
    (LocalEndNode, LocalEndNode) ->
      if Map.null sx && Map.null sy
        then Just (LocalEndNode, [])
        else Nothing
    (LocalSendNode leftPeer _, LocalSendNode rightPeer _)
      | leftPeer == rightPeer
          && Map.keysSet sx == Map.keysSet sy ->
          let labels = Set.toAscList (Map.keysSet sx)
           in do
                plans <- mapM (mergeCommon Send leftPeer sx sy) labels
                pure (LocalSendNode leftPeer labels, plans)
    (LocalRecvNode leftPeer _, LocalRecvNode rightPeer _)
      | leftPeer == rightPeer ->
          let labels = Set.toAscList (Map.keysSet sx `Set.union` Map.keysSet sy)
           in do
                plans <- mapM (mergeUnionReceive leftPeer sx sy) labels
                pure (LocalRecvNode leftPeer labels, plans)
    _ -> Nothing

mergeCommon ::
  LocalDirection ->
  Participant ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Label ->
  Maybe TransitionPlan
mergeCommon direction peer sx sy lbl = do
  (leftEdge, leftDst) <- Map.lookup lbl sx
  (rightEdge, rightDst) <- Map.lookup lbl sy
  guard (edgeMatches direction peer lbl leftEdge)
  guard (edgeMatches direction peer lbl rightEdge)
  let mergedEdge = leftEdge {leTargetHints = mergeHints (leTargetHints leftEdge) (leTargetHints rightEdge)}
  pure (TransitionPlan mergedEdge (Align (Just leftDst) (Just rightDst)))

mergeUnionReceive ::
  Participant ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Label ->
  Maybe TransitionPlan
mergeUnionReceive peer sx sy lbl =
  case (Map.lookup lbl sx, Map.lookup lbl sy) of
    (Just (leftEdge, leftDst), Just (rightEdge, rightDst)) -> do
      guard (edgeMatches Receive peer lbl leftEdge)
      guard (edgeMatches Receive peer lbl rightEdge)
      let mergedEdge = leftEdge {leTargetHints = mergeHints (leTargetHints leftEdge) (leTargetHints rightEdge)}
      pure (TransitionPlan mergedEdge (Align (Just leftDst) (Just rightDst)))
    (Just (leftEdge, leftDst), Nothing) -> do
      guard (edgeMatches Receive peer lbl leftEdge)
      pure (TransitionPlan leftEdge (Align (Just leftDst) Nothing))
    (Nothing, Just (rightEdge, rightDst)) -> do
      guard (edgeMatches Receive peer lbl rightEdge)
      pure (TransitionPlan rightEdge (Align Nothing (Just rightDst)))
    (Nothing, Nothing) -> Nothing

cloneLeft ::
  LocalNode ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Maybe (LocalNode, [TransitionPlan])
cloneLeft node outMap =
  case node of
    LocalEndNode ->
      if Map.null outMap
        then Just (LocalEndNode, [])
        else Nothing
    LocalSendNode peer _ ->
      cloneChoice (LocalSendNode peer) outMap (\dst -> Align (Just dst) Nothing)
    LocalRecvNode peer _ ->
      cloneChoice (LocalRecvNode peer) outMap (\dst -> Align (Just dst) Nothing)
    LocalPayloadSendNode _ _ -> Nothing  -- handled in expandAlign
    LocalPayloadRecvNode _ _ -> Nothing  -- handled in expandAlign

cloneRight ::
  LocalNode ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Maybe (LocalNode, [TransitionPlan])
cloneRight node outMap =
  case node of
    LocalEndNode ->
      if Map.null outMap
        then Just (LocalEndNode, [])
        else Nothing
    LocalSendNode peer _ ->
      cloneChoice (LocalSendNode peer) outMap (\dst -> Align Nothing (Just dst))
    LocalRecvNode peer _ ->
      cloneChoice (LocalRecvNode peer) outMap (\dst -> Align Nothing (Just dst))
    LocalPayloadSendNode _ _ -> Nothing  -- handled in expandAlign
    LocalPayloadRecvNode _ _ -> Nothing  -- handled in expandAlign

cloneChoice ::
  ([Label] -> LocalNode) ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  (G.Vertex -> Align) ->
  Maybe (LocalNode, [TransitionPlan])
cloneChoice mkNode outMap mkAlign = do
  let labels = Set.toAscList (Map.keysSet outMap)
  plans <- mapM mkTransition labels
  pure (mkNode labels, plans)
  where
    mkTransition lbl = do
      (edgeLbl, dst) <- Map.lookup lbl outMap
      pure (TransitionPlan edgeLbl (mkAlign dst))

edgeMatches :: LocalDirection -> Participant -> Label -> LocalEdgeLabel -> Bool
edgeMatches direction peer lbl edge =
  leDirection edge == direction
    && lePeer edge == peer
    && leLabel edge == lbl

validateOutgoing ::
  LocalNode ->
  Map.Map Label (LocalEdgeLabel, G.Vertex) ->
  Maybe ()
validateOutgoing node outMap =
  case node of
    LocalEndNode ->
      guard (Map.null outMap)
    LocalSendNode peer labels -> do
      guard (Set.fromList labels == Map.keysSet outMap)
      mapM_ (check Send peer) (Map.toList outMap)
    LocalRecvNode peer labels -> do
      guard (Set.fromList labels == Map.keysSet outMap)
      mapM_ (check Receive peer) (Map.toList outMap)
    LocalPayloadSendNode _ _ -> guard (Map.null outMap)
    LocalPayloadRecvNode _ _ -> guard (Map.null outMap)
  where
    check direction peer (lbl, (edge, _dst)) =
      guard (edgeMatches direction peer lbl edge)

mergeHints :: RecVarHints -> RecVarHints -> RecVarHints
mergeHints left right =
  RecVarHints
    { rvhBinders = dedupeTypeVars (rvhBinders left ++ rvhBinders right)
    , rvhPreferredVar = rvhPreferredVar left <|> rvhPreferredVar right
    }

dedupeTypeVars :: [TypeVar] -> [TypeVar]
dedupeTypeVars = reverse . fst . foldl' step ([], Set.empty)
  where
    step (acc, seen) tv
      | tv `Set.member` seen = (acc, seen)
      | otherwise = (tv : acc, Set.insert tv seen)

outAt ::
  Map.Map G.Vertex (Map.Map Label (LocalEdgeLabel, G.Vertex)) ->
  G.Vertex ->
  Map.Map Label (LocalEdgeLabel, G.Vertex)
outAt outMap v = Map.findWithDefault Map.empty v outMap

buildMergedGraph :: RecVarHints -> G.Vertex -> MergeBuild -> Maybe LocalGraph
buildMergedGraph startHints startV st = do
  guard (mbNext st > 0)
  let bounds = (0, mbNext st - 1)
      vertices = [0 .. mbNext st - 1]
  nodes <- mapM (\v -> fmap (\node -> (v, node)) (Map.lookup v (mbNodes st))) vertices
  let edges = Set.toList (mbEdges st)
      payloadEdgesList = Set.toList (mbPayloadEdges st)
  pure
    LocalGraph
      { lgGraph = G.buildG bounds (fmap fst edges ++ fmap fst payloadEdgesList)
      , lgStart = startV
      , lgNodes = array bounds nodes
      , lgEdgeLabels = collectEdges edges
      , lgPayloadEdges = collectEdges payloadEdgesList
      , lgStartVarHints = startHints
      }

hoistMaybe :: Maybe a -> MergeM a
hoistMaybe (Just x) = pure x
hoistMaybe Nothing = StateT (const Nothing)

nodeCompatible :: LocalNode -> LocalNode -> Bool
nodeCompatible left right =
  case (left, right) of
    (LocalEndNode, LocalEndNode) -> True
    (LocalSendNode p _, LocalSendNode q _) -> p == q
    (LocalRecvNode p _, LocalRecvNode q _) -> p == q
    (LocalPayloadSendNode p pt, LocalPayloadSendNode q qt) -> p == q && pt == qt
    (LocalPayloadRecvNode p pt, LocalPayloadRecvNode q qt) -> p == q && pt == qt
    _ -> False

successorsByLabel ::
  Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)] ->
  G.Vertex ->
  Maybe (Map.Map Label G.Vertex)
successorsByLabel out v =
  foldl' step (Just Map.empty) (Map.findWithDefault [] v out)
  where
    step Nothing _ = Nothing
    step (Just acc) (edgeLbl, dst) =
      case Map.lookup (leLabel edgeLbl) acc of
        Just _ -> Nothing
        Nothing -> Just (Map.insert (leLabel edgeLbl) dst acc)

outgoingBySource :: Map.Map G.Edge [LocalEdgeLabel] -> Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)]
outgoingBySource =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty

outgoingByLabelMap ::
  Map.Map G.Edge [LocalEdgeLabel] ->
  Maybe (Map.Map G.Vertex (Map.Map Label (LocalEdgeLabel, G.Vertex)))
outgoingByLabelMap =
  Map.foldlWithKey' addEdge (Just Map.empty)
  where
    addEdge acc (from, to) labels = foldl' (insertLabel from to) acc labels

    insertLabel _ _ Nothing _ = Nothing
    insertLabel from to (Just outMap) edgeLbl =
      let byLabel = Map.findWithDefault Map.empty from outMap
          lbl = leLabel edgeLbl
       in case Map.lookup lbl byLabel of
            Nothing ->
              Just (Map.insert from (Map.insert lbl (edgeLbl, to) byLabel) outMap)
            Just (edgeLbl', to')
              | edgeLbl' == edgeLbl && to' == to -> Just outMap
              | otherwise -> Nothing

mergePayloadBoth :: MergeInput -> LocalNode -> G.Vertex -> G.Vertex -> Maybe (LocalNode, [TransitionPlan])
mergePayloadBoth input node lx ry = do
  (lEdge, lDst) <- Map.lookup lx (miLeftPayloadOut input)
  (rEdge, rDst) <- Map.lookup ry (miRightPayloadOut input)
  let mergedEdge = lEdge {lpeTargetHints = mergeHints (lpeTargetHints lEdge) (lpeTargetHints rEdge)}
  pure (node, [PayloadTransitionPlan mergedEdge (Align (Just lDst) (Just rDst))])

clonePayload :: Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex) -> LocalNode -> G.Vertex -> (G.Vertex -> Align) -> Maybe (LocalNode, [TransitionPlan])
clonePayload payloadOut node v mkAlign = do
  (edgeLbl, dst) <- Map.lookup v payloadOut
  pure (node, [PayloadTransitionPlan edgeLbl (mkAlign dst)])

payloadOutgoingBySource :: Map.Map G.Edge [LocalPayloadEdgeLabel] -> Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex)
payloadOutgoingBySource =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insert from (lbl, to) m) acc labels)
    Map.empty

payloadOutgoingByVertex :: Map.Map G.Edge [LocalPayloadEdgeLabel] -> Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex)
payloadOutgoingByVertex =
  Map.foldlWithKey' addEdge Map.empty
  where
    addEdge acc (from, to) labels = foldl' (\m lbl -> Map.insert from (lbl, to) m) acc labels

collectEdges :: Ord k => [(k, v)] -> Map.Map k [v]
collectEdges = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty
