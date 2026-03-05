{-# LANGUAGE DeriveGeneric #-}

-- | Projection algorithms from global graphs to participant-local graphs.
module Project
  ( projectCoinductiveFull
  , projectCoinductivePlain
  , projectInductiveFull
  , projectInductivePlain
  , ProjectionError(..)
  , ProjectionResult
  ) where

import Automata
  ( GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , GlobalNode(..)
  , LocalDirection(..)
  , LocalEdgeLabel(..)
  , LocalGraph(..)
  , LocalNode(..)
  , RecVarHints(..)
  )
import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless)
import GHC.Generics (Generic)
import Control.Monad.State.Strict (gets, modify)
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Data.Array (assocs, array)
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Merge (Merge, bisimMerge, fullMerge, plainMerge)
import Syntax.AST (Label, Participant, TypeVar)

-- | Projection-specific failure.
data ProjectionError = ProjectionError String
  deriving (Eq, Show, Generic)

instance NFData ProjectionError

-- | Result type returned by projection algorithms.
type ProjectionResult = Either ProjectionError LocalGraph

projectCoinductiveFull, projectCoinductivePlain, projectInductiveFull :: GlobalGraph -> Participant -> ProjectionResult
projectCoinductiveFull gg p = do
  env <- buildCoindEnv gg p
  startClosed <- closureFor env (Set.singleton (ggStart gg))
  let st0 =
        CoindState
          { csNext = 1
          , csSeen = Map.singleton startClosed 0
          , csQueue = Seq.singleton startClosed
          , csBuild = emptyBuildGraph
          }
  st <- exploreCoind env st0
  materialiseLocalGraph (csBuild st) (ProjectionTarget 0 (ggStartVarHints gg))

projectCoinductivePlain = projectInductiveWith False bisimMerge
projectInductiveFull = projectInductiveWith True fullMerge

data CoindVertexInfo
  = CoindEnd
  | CoindMessage Participant Participant (Map.Map Label (GlobalEdgeLabel, G.Vertex))
  deriving (Eq, Show)

data CoindEnv = CoindEnv
  { ceParticipant :: Participant
  , ceVertices :: Map.Map G.Vertex CoindVertexInfo
  }

data CoindInvolved = CoindInvolved
  { ciDirection :: !LocalDirection
  , ciPeer :: !Participant
  , ciOutgoing :: Map.Map Label (GlobalEdgeLabel, G.Vertex)
  }

data CoindStep
  = CoindTerminal
  | CoindChoice LocalDirection Participant [(Label, Set.Set G.Vertex, RecVarHints)]

data CoindState = CoindState
  { csNext :: !G.Vertex
  , csSeen :: Map.Map (Set.Set G.Vertex) G.Vertex
  , csQueue :: Seq.Seq (Set.Set G.Vertex)
  , csBuild :: BuildGraph
  }

buildCoindEnv :: GlobalGraph -> Participant -> Either ProjectionError CoindEnv
buildCoindEnv gg p = do
  let nodes = Map.fromList (assocs (ggNodes gg))
      out = globalOutgoing (ggEdgeLabels gg)
  infos <-
    mapM
      (\(v, node) -> do
          info <- buildVertexInfo v node out
          pure (v, info)
      )
      (Map.toList nodes)
  pure
    CoindEnv
      { ceParticipant = p
      , ceVertices = Map.fromList infos
      }

buildVertexInfo ::
  G.Vertex ->
  GlobalNode ->
  Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)] ->
  Either ProjectionError CoindVertexInfo
buildVertexInfo v node out =
  case node of
    GlobalEndNode ->
      case Map.lookup v out of
        Nothing -> Right CoindEnd
        Just _ ->
          Left
            ( ProjectionError
                ( "Invalid global graph: end vertex "
                    ++ show v
                    ++ " has outgoing transitions."
                )
            )
    GlobalNode -> do
      branches <- outgoingAt v out
      if null branches
        then
          Left
            ( ProjectionError
                ( "Invalid global graph: message vertex "
                    ++ show v
                    ++ " has no outgoing transitions."
                )
            )
        else do
          let firstLabel = fst (head branches)
              sender = geSender firstLabel
              receiver = geReceiver firstLabel
          unless (all (\(e, _) -> geSender e == sender && geReceiver e == receiver) branches) $
            Left
              ( ProjectionError
                  ( "Invalid global graph: vertex "
                      ++ show v
                      ++ " mixes multiple sender/receiver pairs."
                  )
              )
          outMap <- foldM insertBranch Map.empty branches
          pure (CoindMessage sender receiver outMap)
  where
    insertBranch acc (edgeLbl, dst)
      | Map.member (geLabel edgeLbl) acc =
          Left
            ( ProjectionError
                ( "Invalid global graph: duplicate branch label "
                    ++ show (geLabel edgeLbl)
                    ++ " at vertex "
                    ++ show v
                    ++ "."
                )
            )
      | otherwise =
          Right (Map.insert (geLabel edgeLbl) (edgeLbl, dst) acc)

closureFor :: CoindEnv -> Set.Set G.Vertex -> Either ProjectionError (Set.Set G.Vertex)
closureFor env seeds = go seeds (Set.toList seeds)
  where
    go closed [] = Right closed
    go closed (v : pending) = do
      info <- lookupCoindVertex env v
      case info of
        CoindEnd -> go closed pending
        CoindMessage sender receiver outMap
          | ceParticipant env == sender || ceParticipant env == receiver ->
              go closed pending
          | otherwise ->
              let succs = fmap snd (Map.elems outMap)
                  (closed', pending') = addAll succs closed pending
               in go closed' pending'

    addAll [] closed pending = (closed, pending)
    addAll (x : xs) closed pending
      | x `Set.member` closed = addAll xs closed pending
      | otherwise = addAll xs (Set.insert x closed) (x : pending)

exploreCoind :: CoindEnv -> CoindState -> Either ProjectionError CoindState
exploreCoind env st =
  case Seq.viewl (csQueue st) of
    Seq.EmptyL -> Right st
    stateSet Seq.:< rest -> do
      from <- lookupSeenState st stateSet
      let st1 = st {csQueue = rest}
      step <- analyseClosedState env stateSet
      st2 <-
        case step of
          CoindTerminal -> do
            build <- ensureNode from LocalEndNode (csBuild st1)
            pure st1 {csBuild = build}
          CoindChoice direction peer transitions -> do
            let labels = fmap (\(lbl, _, _) -> lbl) transitions
                node =
                  case direction of
                    Send -> LocalSendNode peer labels
                    Receive -> LocalRecvNode peer labels
            build0 <- ensureChoiceNode True from node (csBuild st1)
            foldM (stepTransition from direction peer) (st1 {csBuild = build0}) transitions
      exploreCoind env st2
  where
    stepTransition from direction peer stNow (lbl, nextRaw, hints) = do
      closedNext <- closureFor env nextRaw
      let (to, stWithSeen) = ensureSeenState closedNext stNow
          edgeLbl = LocalEdgeLabel direction peer lbl hints
      build <- insertEdge from to edgeLbl (csBuild stWithSeen)
      pure stWithSeen {csBuild = build}

lookupSeenState :: CoindState -> Set.Set G.Vertex -> Either ProjectionError G.Vertex
lookupSeenState st stateSet =
  maybe
    ( Left
        ( ProjectionError
            ( "Projection internal error: missing coinductive state id for "
                ++ show (Set.toList stateSet)
                ++ "."
            )
        )
    )
    Right
    (Map.lookup stateSet (csSeen st))

ensureSeenState :: Set.Set G.Vertex -> CoindState -> (G.Vertex, CoindState)
ensureSeenState stateSet st =
  case Map.lookup stateSet (csSeen st) of
    Just v -> (v, st)
    Nothing ->
      let v = csNext st
       in ( v
          , st
              { csNext = v + 1
              , csSeen = Map.insert stateSet v (csSeen st)
              , csQueue = csQueue st Seq.|> stateSet
              }
          )

analyseClosedState :: CoindEnv -> Set.Set G.Vertex -> Either ProjectionError CoindStep
analyseClosedState env stateSet = do
  (hasEnd, involved) <- foldM step (False, []) (Set.toList stateSet)
  case involved of
    [] -> Right CoindTerminal
    first : rest -> do
      unless (not hasEnd) $
        Left
          ( ProjectionError
              ( "Coinductive-full projection failed: closed state "
                  ++ show (Set.toList stateSet)
                  ++ " mixes end with participant-involved actions."
              )
          )
      unless (all (consistentWith first) rest) $
        Left
          ( ProjectionError
              ( "Coinductive-full projection failed: closed state "
                  ++ show (Set.toList stateSet)
                  ++ " mixes incompatible role/peer actions for participant "
                  ++ show (ceParticipant env)
                  ++ "."
              )
          )
      case ciDirection first of
        Send -> analyseSend stateSet first rest
        Receive -> analyseRecv first rest
  where
    step (hasEnd, acc) v = do
      info <- lookupCoindVertex env v
      case info of
        CoindEnd -> pure (True, acc)
        CoindMessage sender receiver outMap
          | ceParticipant env == sender ->
              pure
                ( hasEnd
                , CoindInvolved Send receiver outMap : acc
                )
          | ceParticipant env == receiver ->
              pure
                ( hasEnd
                , CoindInvolved Receive sender outMap : acc
                )
          | otherwise ->
              pure (hasEnd, acc)

    consistentWith first other =
      ciDirection first == ciDirection other
        && ciPeer first == ciPeer other

    analyseSend closedSet first rest = do
      let infos = first : rest
          firstLabels = Map.keysSet (ciOutgoing first)
      unless (all (\i -> Map.keysSet (ciOutgoing i) == firstLabels) rest) $
        Left
          ( ProjectionError
              ( "Coinductive-full projection failed: closed state "
                  ++ show (Set.toList closedSet)
                  ++ " has send nodes with different label sets."
              )
          )
      let labels = Set.toAscList firstLabels
          transitions = fmap (mkTransition infos) labels
      pure (CoindChoice Send (ciPeer first) transitions)

    analyseRecv first rest =
      let infos = first : rest
          allLabels =
            foldl'
              (\acc i -> acc `Set.union` Map.keysSet (ciOutgoing i))
              Set.empty
              infos
          transitions = fmap (mkTransition infos) (Set.toAscList allLabels)
       in Right (CoindChoice Receive (ciPeer first) transitions)

    mkTransition infos lbl =
      let picks = mapMaybe (Map.lookup lbl . ciOutgoing) infos
          nextSet = Set.fromList (fmap snd picks)
          hints = foldl' appendHints emptyHints (fmap (geTargetHints . fst) picks)
       in (lbl, nextSet, hints)

lookupCoindVertex :: CoindEnv -> G.Vertex -> Either ProjectionError CoindVertexInfo
lookupCoindVertex env v =
  maybe
    (Left (ProjectionError ("Global vertex " ++ show v ++ " has no node metadata.")))
    Right
    (Map.lookup v (ceVertices env))

emptyHints :: RecVarHints
emptyHints = RecVarHints [] Nothing

-- | Inductive/plain projection is inductive projection parameterised by
-- plain merge (isomorphic branches only).
projectInductivePlain :: GlobalGraph -> Participant -> ProjectionResult
projectInductivePlain = projectInductiveWith False plainMerge

-- | Generic inductive projection parameterised by a branch-merge operator.
projectInductiveWith :: Bool -> Merge -> GlobalGraph -> Participant -> ProjectionResult
projectInductiveWith allowRecvUnion mergeFn gg p = do
  let env =
        ProjEnv
          { peParticipant = p
          , peAllowRecvUnion = allowRecvUnion
          , peMerge = mergeFn
          , peGlobalNodes = Map.fromList (assocs (ggNodes gg))
          , peOutgoing = globalOutgoing (ggEdgeLabels gg)
          }
      st0 = ProjState 1 Map.empty emptyBuildGraph
  (target, st) <- runStateT (projectAt env Set.empty (ggStartVarHints gg) (ggStart gg) 0) st0
  materialiseLocalGraph (psBuild st) target

data ProjectionAction = ProjectSend Participant | ProjectReceive Participant
  deriving (Eq, Show)

data ProjectionTarget = ProjectionTarget
  { ptVertex :: !G.Vertex
  , ptHints :: RecVarHints
  } deriving (Eq, Show)

data BuildGraph = BuildGraph
  { bgNodes :: Map.Map G.Vertex LocalNode
  , bgEdges :: [((G.Vertex, G.Vertex), LocalEdgeLabel)]
  }

emptyBuildGraph :: BuildGraph
emptyBuildGraph = BuildGraph Map.empty []

data ProjEnv = ProjEnv
  { peParticipant :: Participant
  , peAllowRecvUnion :: Bool
  , peMerge :: Merge
  , peGlobalNodes :: Map.Map G.Vertex GlobalNode
  , peOutgoing :: Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)]
  }

data ProjState = ProjState
  { psNextFresh :: !G.Vertex
  , psGlobalToLocal :: Map.Map G.Vertex G.Vertex
  , psBuild :: BuildGraph
  }

type ProjM a = StateT ProjState (Either ProjectionError) a

-- | DFS projection from global vertex @gv@ into local start @lv@.
projectAt :: ProjEnv -> Set.Set G.Vertex -> RecVarHints -> G.Vertex -> G.Vertex -> ProjM ProjectionTarget
projectAt env ignored hints gv lv = do
  mapped <- gets (Map.lookup gv . psGlobalToLocal)
  case mapped of
    Just lv' -> pure (ProjectionTarget lv' hints)
    Nothing -> do
      modify (\s -> s {psGlobalToLocal = Map.insert gv lv (psGlobalToLocal s)})
      node <- liftEither $ lookupGlobalNode (peGlobalNodes env) gv
      case node of
        GlobalEndNode -> do
          ensureNodeM lv LocalEndNode
          pure (ProjectionTarget lv hints)
        GlobalNode -> do
          branches <- liftEither $ outgoingAt gv (peOutgoing env)
          action <- liftEither $ actionFor (peParticipant env) gv branches
          case action of
            Just a -> projectInvolved env hints lv a branches
            Nothing -> projectIgnored env ignored hints gv lv branches

projectInvolved :: ProjEnv -> RecVarHints -> G.Vertex -> ProjectionAction -> [(GlobalEdgeLabel, G.Vertex)] -> ProjM ProjectionTarget
projectInvolved env hints lv action branches = do
  let sorted = sortOn (\(lbl, dst) -> (geLabel lbl, dst)) branches
      labels = fmap (geLabel . fst) sorted
      node =
        case action of
          ProjectSend peer -> LocalSendNode peer labels
          ProjectReceive peer -> LocalRecvNode peer labels
  ensureChoiceNodeM (peAllowRecvUnion env) lv node
  mapM_ step sorted
  pure (ProjectionTarget lv hints)
  where
    step (edgeLbl, child) = do
      existing <-
        gets
          ( existingEdgeTarget
              (actionDirection action)
              (actionPeer action)
              (geLabel edgeLbl)
              lv
              . psBuild
          )
      childStart <- maybe freshLocalVertex pure existing
      target <- projectAt env Set.empty (geTargetHints edgeLbl) child childStart
      let lbl =
            LocalEdgeLabel
              (actionDirection action)
              (actionPeer action)
              (geLabel edgeLbl)
              (ptHints target)
      insertEdgeM lv (ptVertex target) lbl

-- | Ignore a global node and merge all branch projections with 'peMerge'.
projectIgnored :: ProjEnv -> Set.Set G.Vertex -> RecVarHints -> G.Vertex -> G.Vertex -> [(GlobalEdgeLabel, G.Vertex)] -> ProjM ProjectionTarget
projectIgnored env ignored hints gv lv branches =
  if gv `Set.member` ignored
    then do
      -- Uninvolved cycle: cut branch and terminate locally.
      ensureNodeM lv LocalEndNode
      pure (ProjectionTarget lv hints)
    else do
      let sorted = sortOn (\(lbl, dst) -> (geLabel lbl, dst)) branches
      targets <-
        mapM
          (\(edgeLbl, child) ->
              projectAt
                env
                (Set.insert gv ignored)
                (appendHints hints (geTargetHints edgeLbl))
                child
                lv
          )
          sorted
      mergeIgnoredTargetsM env gv targets

mergeIgnoredTargetsM :: ProjEnv -> G.Vertex -> [ProjectionTarget] -> ProjM ProjectionTarget
mergeIgnoredTargetsM env gv targets =
  case targets of
    [] ->
      failProjection ("Invalid global graph: ignored vertex " ++ show gv ++ " has no outgoing branches.")
    [one] -> pure one
    firstT : rest -> do
      build <- gets psBuild
      firstG <- liftEither $ materialiseLocalGraph build firstT
      _ <- foldM (mergeOne build (ptHints firstT)) firstG rest
      pure firstT
  where
    mergeOne build expectedHints accGraph otherT = do
      unless (ptHints otherT == expectedHints) $
        failProjection
          ( "Inductive projection failed at ignored vertex "
              ++ show gv
              ++ ": branches carry different hint annotations."
          )
      otherG <- liftEither $ materialiseLocalGraph build otherT
      case peMerge env accGraph otherG of
        Nothing ->
          failProjection
            ( "Inductive projection failed at ignored vertex "
                ++ show gv
                ++ ": branch projections cannot be merged."
            )
        Just merged ->
          pure merged

actionFor :: Participant -> G.Vertex -> [(GlobalEdgeLabel, G.Vertex)] -> Either ProjectionError (Maybe ProjectionAction)
actionFor p v branches =
  case fmap (branchAction p . fst) branches of
    [] ->
      Left $ ProjectionError ("Invalid global graph: message vertex " ++ show v ++ " has no outgoing transitions.")
    acts
      | all (== Nothing) acts -> Right Nothing
      | otherwise ->
          case sequence acts of
            Just (a : as) | all (== a) as -> Right (Just a)
            Just _ ->
              Left
                ( ProjectionError
                    ( "Inductive projection failed: participant "
                        ++ show p
                        ++ " has inconsistent role at vertex "
                        ++ show v
                        ++ "."
                    )
                )
            Nothing ->
              Left
                ( ProjectionError
                    ( "Inductive projection failed: participant "
                        ++ show p
                        ++ " appears in only some branches at vertex "
                        ++ show v
                        ++ "."
                    )
                )

branchAction :: Participant -> GlobalEdgeLabel -> Maybe ProjectionAction
branchAction p e
  | geSender e == p = Just (ProjectSend (geReceiver e))
  | geReceiver e == p = Just (ProjectReceive (geSender e))
  | otherwise = Nothing

actionDirection :: ProjectionAction -> LocalDirection
actionDirection (ProjectSend _) = Send
actionDirection (ProjectReceive _) = Receive

actionPeer :: ProjectionAction -> Participant
actionPeer (ProjectSend peer) = peer
actionPeer (ProjectReceive peer) = peer

appendHints :: RecVarHints -> RecVarHints -> RecVarHints
appendHints left right =
  RecVarHints
    { rvhBinders = dedupeTypeVars (rvhBinders left ++ rvhBinders right)
    , rvhPreferredVar = case rvhPreferredVar right of
        Just tv -> Just tv
        Nothing -> rvhPreferredVar left
    }

dedupeTypeVars :: [TypeVar] -> [TypeVar]
dedupeTypeVars = reverse . fst . foldl' step ([], Set.empty)
  where
    step (acc, seen) tv
      | tv `Set.member` seen = (acc, seen)
      | otherwise = (tv : acc, Set.insert tv seen)

lookupGlobalNode :: Map.Map G.Vertex GlobalNode -> G.Vertex -> Either ProjectionError GlobalNode
lookupGlobalNode nodes v =
  maybe
    (Left (ProjectionError ("Global vertex " ++ show v ++ " has no node metadata.")))
    Right
    (Map.lookup v nodes)

outgoingAt :: G.Vertex -> Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)] -> Either ProjectionError [(GlobalEdgeLabel, G.Vertex)]
outgoingAt v out =
  maybe
    (Left (ProjectionError ("Invalid global graph: message vertex " ++ show v ++ " has no outgoing transitions.")))
    Right
    (Map.lookup v out)

globalOutgoing :: Map.Map G.Edge [GlobalEdgeLabel] -> Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)]
globalOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty

freshLocalVertex :: ProjM G.Vertex
freshLocalVertex = do
  v <- gets psNextFresh
  modify (\s -> s {psNextFresh = v + 1})
  pure v

ensureNodeM :: G.Vertex -> LocalNode -> ProjM ()
ensureNodeM v node = do
  build <- gets psBuild
  build' <- liftEither $ ensureNode v node build
  modify (\s -> s {psBuild = build'})

ensureChoiceNodeM :: Bool -> G.Vertex -> LocalNode -> ProjM ()
ensureChoiceNodeM allowRecvUnion v node = do
  build <- gets psBuild
  build' <- liftEither $ ensureChoiceNode allowRecvUnion v node build
  modify (\s -> s {psBuild = build'})

insertEdgeM :: G.Vertex -> G.Vertex -> LocalEdgeLabel -> ProjM ()
insertEdgeM from to lbl = do
  build <- gets psBuild
  build' <- liftEither $ insertEdge from to lbl build
  modify (\s -> s {psBuild = build'})

ensureNode :: G.Vertex -> LocalNode -> BuildGraph -> Either ProjectionError BuildGraph
ensureNode v node build =
  case Map.lookup v (bgNodes build) of
    Nothing -> Right build {bgNodes = Map.insert v node (bgNodes build)}
    Just same | same == node -> Right build
    Just _ ->
      Left
        ( ProjectionError
            ( "Projection conflict at local vertex "
                ++ show v
                ++ ": incompatible node metadata."
            )
        )

ensureChoiceNode :: Bool -> G.Vertex -> LocalNode -> BuildGraph -> Either ProjectionError BuildGraph
ensureChoiceNode allowRecvUnion v node build =
  case Map.lookup v (bgNodes build) of
    Nothing -> Right build {bgNodes = Map.insert v node (bgNodes build)}
    Just same
      | same == node -> Right build
      | allowRecvUnion ->
          case (same, node) of
            (LocalRecvNode peerOld labelsOld, LocalRecvNode peerNew labelsNew)
              | peerOld == peerNew ->
                  let merged = LocalRecvNode peerOld (Set.toAscList (Set.fromList labelsOld `Set.union` Set.fromList labelsNew))
                   in Right build {bgNodes = Map.insert v merged (bgNodes build)}
            _ ->
              projectionConflict
      | otherwise ->
          projectionConflict
  where
    projectionConflict =
      Left
        ( ProjectionError
            ( "Projection conflict at local vertex "
                ++ show v
                ++ ": incompatible node metadata."
            )
        )

-- | Insert branch edge. Same source+label must agree on target+metadata.
insertEdge :: G.Vertex -> G.Vertex -> LocalEdgeLabel -> BuildGraph -> Either ProjectionError BuildGraph
insertEdge from to lbl build =
  case matches of
    [] -> Right build {bgEdges = ((from, to), lbl) : bgEdges build}
    _ ->
      if any (\(to', lbl') -> to' == to && lbl' == lbl) matches
        then Right build
        else
          Left
            ( ProjectionError
                ( "Projection conflict at local vertex "
                    ++ show from
                    ++ ": duplicate branch label "
                    ++ show (leLabel lbl)
                    ++ " with incompatible targets."
                )
            )
  where
    matches =
      [ (to', lbl')
      | ((from', to'), lbl') <- bgEdges build
      , from' == from
      , leDirection lbl' == leDirection lbl
      , lePeer lbl' == lePeer lbl
      , leLabel lbl' == leLabel lbl
      ]

existingEdgeTarget :: LocalDirection -> Participant -> Label -> G.Vertex -> BuildGraph -> Maybe G.Vertex
existingEdgeTarget direction peer label from build =
  case matches of
    ((to, _) : _) -> Just to
    [] -> Nothing
  where
    matches =
      [ (to', lbl')
      | ((from', to'), lbl') <- bgEdges build
      , from' == from
      , leDirection lbl' == direction
      , lePeer lbl' == peer
      , leLabel lbl' == label
      ]

materialiseLocalGraph :: BuildGraph -> ProjectionTarget -> Either ProjectionError LocalGraph
materialiseLocalGraph build target = do
  _ <- requireNode oldStart (bgNodes build)
  let reachable = bfsReachable (localOutgoing (bgEdges build)) oldStart
      renaming = Map.fromList (zip reachable [0 ..])
      n = length reachable
  unless (n > 0) $
    Left (ProjectionError "Projection produced an empty local graph.")
  nodes <-
    mapM
      (\oldV -> do
          node <- requireNode oldV (bgNodes build)
          pure (renaming Map.! oldV, node)
      )
      reachable
  let edges =
        [ ((renaming Map.! from, renaming Map.! to), lbl)
        | ((from, to), lbl) <- bgEdges build
        , from `Map.member` renaming
        , to `Map.member` renaming
        ]
      arrBounds = (0, n - 1)
  pure
    LocalGraph
      { lgGraph = G.buildG arrBounds (fmap fst edges)
      , lgStart = renaming Map.! oldStart
      , lgNodes = array arrBounds nodes
      , lgEdgeLabels = collectEdges edges
      , lgStartVarHints = ptHints target
      }
  where
    oldStart = ptVertex target

requireNode :: G.Vertex -> Map.Map G.Vertex LocalNode -> Either ProjectionError LocalNode
requireNode v nodes =
  maybe
    (Left (ProjectionError ("Projection internal error: missing local node metadata for vertex " ++ show v ++ ".")))
    Right
    (Map.lookup v nodes)

localOutgoing :: [((G.Vertex, G.Vertex), LocalEdgeLabel)] -> Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)]
localOutgoing =
  foldl' (\acc ((from, to), lbl) -> Map.insertWith (++) from [(lbl, to)] acc) Map.empty

bfsReachable :: Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)] -> G.Vertex -> [G.Vertex]
bfsReachable out start = go Set.empty [start] []
  where
    go _ [] acc = reverse acc
    go seen (v : vs) acc
      | v `Set.member` seen = go seen vs acc
      | otherwise =
          let succs = fmap snd (Map.findWithDefault [] v out)
           in go (Set.insert v seen) (vs ++ succs) (v : acc)

collectEdges :: Ord k => [(k, v)] -> Map.Map k [v]
collectEdges = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

liftEither :: Either ProjectionError a -> ProjM a
liftEither (Right x) = pure x
liftEither (Left err) = StateT (\_ -> Left err)

failProjection :: String -> ProjM a
failProjection msg = liftEither (Left (ProjectionError msg))
