-- | Constraint-based type inference for processes.
--
-- Infers a 'LocalType' from a 'Process' alone using constraint generation
-- (fresh type variables + lower-bound constraints) followed by a graph-based
-- solver that builds a 'LocalGraph' (powerset/subset construction over
-- inference variables) and converts it to a 'LocalType' via
-- 'localGraphToType'.
module Infer
  ( InferError(..)
  , infer
  ) where

import Control.Monad (when)
import Control.Monad.State.Strict
import Data.Array (array)
import Data.Foldable (foldl')
import qualified Data.Graph as G
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST
import Syntax.AlphaEq (normalizeLocalBranchOrder)
import Automata (LocalGraph(..), LocalNode(..), LocalDirection(..),
                 LocalEdgeLabel(..), LocalPayloadEdgeLabel(..),
                 emptyRecVarHints, localGraphToType)
import Typecheck (ExprType(..), inferExprType)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

type InferVar = Int

data InferError
  = InferActionMismatch         -- ^ Node has mixed send/recv/end bounds
  | InferParticipantMismatch Participant Participant
  | InferEmptyRecvIntersection
  | InferConditionNotBool Expr ExprType
  | InferUnboundProcessVar TypeVar
  | InferGraphConversionError String
  | InferUndeterminedPayloadType  -- ^ Payload type cannot be inferred (e.g. PRecvPayload, EVar in send)
  deriving (Eq, Show)

data StructBound
  = BEnd
  | BSend Participant Label InferVar
  | BRecv Participant [(Label, InferVar)]
  | BPayloadSend Participant PayloadType InferVar
  | BPayloadRecv Participant PayloadType InferVar

data Constraint
  = CStruct StructBound InferVar
  | CVarBound InferVar InferVar

------------------------------------------------------------------------
-- Phase 1: Constraint generation
------------------------------------------------------------------------

data GenState = GenState
  { gsNextVar     :: !InferVar
  , gsConstraints :: [Constraint]
  , gsErrors      :: [InferError]
  }

type GenM = State GenState

type GenEnv = Map.Map TypeVar InferVar

freshVar :: GenM InferVar
freshVar = do
  s <- get
  let v = gsNextVar s
  put s { gsNextVar = v + 1 }
  return v

emitConstraint :: Constraint -> GenM ()
emitConstraint c = modify' $ \s -> s { gsConstraints = c : gsConstraints s }

emitError :: InferError -> GenM ()
emitError e = modify' $ \s -> s { gsErrors = e : gsErrors s }

generate :: GenEnv -> Process -> GenM InferVar
generate env proc =
  case proc of
    PEnd -> do
      xi <- freshVar
      emitConstraint (CStruct BEnd xi)
      return xi

    PVar v ->
      case Map.lookup v env of
        Nothing -> do
          emitError (InferUnboundProcessVar v)
          xi <- freshVar
          emitConstraint (CStruct BEnd xi)
          return xi
        Just psi -> do
          xi <- freshVar
          emitConstraint (CVarBound psi xi)
          return xi

    PRec v body -> do
      xi <- freshVar
      let env' = Map.insert v xi env
      bodyVar <- generate env' body
      emitConstraint (CVarBound bodyVar xi)
      return xi

    PSend p l cont -> do
      psi <- generate env cont
      xi <- freshVar
      emitConstraint (CStruct (BSend p l psi) xi)
      return xi

    PRecv p branches -> do
      branchVars <- mapM (\(lbl, body) -> do
        psi <- generate env body
        return (lbl, psi)) (NE.toList branches)
      xi <- freshVar
      emitConstraint (CStruct (BRecv p branchVars) xi)
      return xi

    PSendPayload p e cont ->
      case exprToPayloadType e of
        Nothing -> do
          emitError InferUndeterminedPayloadType
          xi <- freshVar
          emitConstraint (CStruct BEnd xi)
          return xi
        Just pt -> do
          psi <- generate env cont
          xi <- freshVar
          emitConstraint (CStruct (BPayloadSend p pt psi) xi)
          return xi

    PRecvPayload _p _var _cont -> do
      -- PRecvPayload does not carry a payload type annotation, so we
      -- cannot determine it from the process alone.
      emitError InferUndeterminedPayloadType
      xi <- freshVar
      emitConstraint (CStruct BEnd xi)
      return xi

    PIf e thenP elseP -> do
      case inferExprType e of
        Left _err ->
          emitError (InferConditionNotBool e TAny)
        Right ety ->
          when (ety /= TBool && ety /= TAny) $
            emitError (InferConditionNotBool e ety)
      psi1 <- generate env thenP
      psi2 <- generate env elseP
      xi <- freshVar
      emitConstraint (CVarBound psi1 xi)
      emitConstraint (CVarBound psi2 xi)
      return xi

-- | Try to determine a PayloadType from an expression.
-- Returns Nothing for expression variables and other undetermined cases.
exprToPayloadType :: Expr -> Maybe PayloadType
exprToPayloadType EUnit = Just PTUnit
exprToPayloadType e =
  case inferExprType e of
    Right TInt  -> Just PTInt
    Right TBool -> Just PTBool
    _           -> Nothing

------------------------------------------------------------------------
-- Phase 2: Graph-based constraint solving
------------------------------------------------------------------------
--
-- The solver has three steps:
--
-- Step 2a — Equivalence classes.  Every CVarBound ψ ξ equates two
--   inference variables: PVar aliases (ψ = rec-var, ξ = fresh),
--   recursive fixpoints (ψ = body, ξ = rec-var), and conditional merge
--   points (ψ = branch, ξ = join var).  We compute connected components
--   of the undirected CVarBound graph and pick a canonical representative
--   (smallest var) for each class.
--
-- Step 2b — Canonicalize structural bounds.  Each CStruct bound is
--   rewritten so that continuation variables use class representatives.
--
-- Step 2c — BFS powerset construction.  Each graph node is a set of
--   class representatives.  For most protocols the set is a singleton
--   (all "parallel" variables collapse into one class).  For protocols
--   whose conditional branches have different recursion depths, a
--   label's continuation vars may span multiple classes, creating a
--   composite node whose cycle length is the LCM of the periods.

type ClassRep = InferVar

-- | Structural bound with continuation variables canonicalized to class
-- representatives.
data CanonBound
  = CBEnd
  | CBSend Participant Label ClassRep
  | CBRecv Participant [(Label, ClassRep)]
  | CBPayloadSend Participant PayloadType ClassRep
  | CBPayloadRecv Participant PayloadType ClassRep

-- | Solver environment after equivalence-class computation.
data SolveEnv = SolveEnv
  { seCanonMap    :: Map.Map InferVar ClassRep       -- ^ var → class rep
  , seClassBounds :: Map.Map ClassRep [CanonBound]   -- ^ class rep → bounds
  }

-- | Canonical representative of a variable's equivalence class.
canonVar :: SolveEnv -> InferVar -> ClassRep
canonVar env v = Map.findWithDefault v v (seCanonMap env)

buildSolveEnv :: [Constraint] -> SolveEnv
buildSolveEnv constraints =
  let -- Bidirectional adjacency list for CVarBound edges
      adj = foldl' addEdge Map.empty constraints
      addEdge m (CVarBound psi xi) =
        Map.insertWith (++) psi [xi] $
        Map.insertWith (++) xi [psi] m
      addEdge m _ = m
      -- Connected components → canonical representative map
      canonMap = computeCanonMap adj (Map.keys adj)
      f v = Map.findWithDefault v v canonMap
      -- Collect struct bounds per class, canonicalizing continuation vars
      classBounds = foldl' addBound Map.empty constraints
      addBound m (CStruct sb xi) =
        Map.insertWith (++) (f xi) [canonBound f sb] m
      addBound m _ = m
  in SolveEnv canonMap classBounds

canonBound :: (InferVar -> ClassRep) -> StructBound -> CanonBound
canonBound _ BEnd = CBEnd
canonBound f (BSend p l v) = CBSend p l (f v)
canonBound f (BRecv p branches) = CBRecv p [(l, f v) | (l, v) <- branches]
canonBound f (BPayloadSend p pt v) = CBPayloadSend p pt (f v)
canonBound f (BPayloadRecv p pt v) = CBPayloadRecv p pt (f v)

-- | Compute connected components via BFS.  Returns a map from each
-- variable to the smallest variable in its component.
computeCanonMap :: Map.Map InferVar [InferVar] -> [InferVar]
                -> Map.Map InferVar ClassRep
computeCanonMap adj = fst . foldl' visit (Map.empty, Set.empty)
  where
    visit (cm, seen) v
      | Set.member v seen = (cm, seen)
      | otherwise =
          let comp = bfs (Set.singleton v) Set.empty
              rep  = Set.findMin comp
          in (Map.union cm (Map.fromSet (const rep) comp), Set.union seen comp)
    bfs frontier visited
      | Set.null frontier = visited
      | otherwise =
          let visited' = Set.union visited frontier
              next = Set.fromList
                [ n | u <- Set.toList frontier
                    , n <- Map.findWithDefault [] u adj
                    , not (Set.member n visited')
                ]
          in bfs next visited'

-- | Node key: a set of equivalence-class representatives.
-- Singleton for most protocols; multi-element only for different
-- recursion depths in conditional branches.
type NodeKey = Set.Set ClassRep

-- | Classified action of a node.
data NodeAction
  = NAEnd
  | NASend Participant (Map.Map Label (Set.Set ClassRep))
  | NARecv Participant (Map.Map Label (Set.Set ClassRep))
  | NAPayloadSend Participant PayloadType (Set.Set ClassRep)
  | NAPayloadRecv Participant PayloadType (Set.Set ClassRep)

-- | Classify the canonical bounds of a node.
classifyNode :: SolveEnv -> NodeKey -> Either InferError NodeAction
classifyNode env key =
  let allBounds = concatMap
        (\rep -> Map.findWithDefault [] rep (seClassBounds env))
        (Set.toList key)
  in case allBounds of
    [] -> Right NAEnd
    _  -> classifyCanonBounds allBounds

classifyCanonBounds :: [CanonBound] -> Either InferError NodeAction
classifyCanonBounds bounds =
  let ends     = [() | CBEnd <- bounds]
      sends    = [(p, l, v) | CBSend p l v <- bounds]
      recvs    = [(p, branches) | CBRecv p branches <- bounds]
      plSends  = [(p, pt, v) | CBPayloadSend p pt v <- bounds]
      plRecvs  = [(p, pt, v) | CBPayloadRecv p pt v <- bounds]
  in case (ends, sends, recvs, plSends, plRecvs) of
    -- All end
    (_:_, [], [], [], []) -> Right NAEnd
    -- All label sends
    ([], _:_, [], [], []) ->
      let participants = map (\(p,_,_) -> p) sends
          p0 = head participants
      in if all (== p0) participants
         then
           let labelMap = foldl'
                 (\m (_, l, v) -> Map.insertWith Set.union l (Set.singleton v) m)
                 Map.empty sends
           in Right (NASend p0 labelMap)
         else Left (InferParticipantMismatch p0 (head (filter (/= p0) participants)))
    -- All label recvs
    ([], [], _:_, [], []) ->
      let participants = map fst recvs
          p0 = head participants
      in if all (== p0) participants
         then
           let allBranches = concatMap snd recvs
               fullMap = foldl'
                 (\m (l, v) -> Map.insertWith Set.union l (Set.singleton v) m)
                 Map.empty allBranches
               labelSets = map (Set.fromList . map fst . snd) recvs
               commonLabels = foldl1 Set.intersection labelSets
           in if Set.null commonLabels
              then Left InferEmptyRecvIntersection
              else Right (NARecv p0 (Map.restrictKeys fullMap commonLabels))
         else Left (InferParticipantMismatch p0 (head (filter (/= p0) participants)))
    -- All payload sends (must agree on participant and payload type)
    ([], [], [], _:_, []) ->
      let participants = map (\(p,_,_) -> p) plSends
          payloads    = map (\(_,pt,_) -> pt) plSends
          contReps    = map (\(_,_,v) -> v) plSends
          p0 = head participants
          pt0 = head payloads
      in if all (== p0) participants && all (== pt0) payloads
         then Right (NAPayloadSend p0 pt0 (Set.fromList contReps))
         else Left InferActionMismatch
    -- All payload recvs (must agree on participant and payload type)
    ([], [], [], [], _:_) ->
      let participants = map (\(p,_,_) -> p) plRecvs
          payloads    = map (\(_,pt,_) -> pt) plRecvs
          contReps    = map (\(_,_,v) -> v) plRecvs
          p0 = head participants
          pt0 = head payloads
      in if all (== p0) participants && all (== pt0) payloads
         then Right (NAPayloadRecv p0 pt0 (Set.fromList contReps))
         else Left InferActionMismatch
    -- Any mixture is an error
    _ -> Left InferActionMismatch

-- | BFS state for building the graph.
data BFSState = BFSState
  { bfsNodeMap      :: Map.Map NodeKey G.Vertex
  , bfsNextVtx      :: G.Vertex
  , bfsNodes        :: [(G.Vertex, LocalNode)]
  , bfsEdges        :: [(G.Edge, LocalEdgeLabel)]
  , bfsPayloadEdges :: [(G.Edge, LocalPayloadEdgeLabel)]
  , bfsQueue        :: [NodeKey]
  , bfsErrors       :: [InferError]
  }

-- | Successor descriptor: either a branch edge or a payload edge.
data Successor
  = BranchSucc Label LocalDirection Participant (Set.Set ClassRep)
  | PayloadSucc LocalDirection Participant PayloadType (Set.Set ClassRep)

-- | Build a 'LocalGraph' from constraints via BFS over class-rep sets.
solveConstraints :: SolveEnv -> InferVar -> Either [InferError] LocalGraph
solveConstraints env rootVar =
  let rootKey = Set.singleton (canonVar env rootVar)
      initState = BFSState
        { bfsNodeMap = Map.singleton rootKey 0
        , bfsNextVtx = 1
        , bfsNodes = []
        , bfsEdges = []
        , bfsPayloadEdges = []
        , bfsQueue = [rootKey]
        , bfsErrors = []
        }
      finalSt = bfsLoop env initState
  in case bfsErrors finalSt of
    errs@(_:_) -> Left errs
    [] ->
      let n = bfsNextVtx finalSt
          nodeArr = array (0, n - 1) (bfsNodes finalSt)
          allEdges = map fst (bfsEdges finalSt) ++ map fst (bfsPayloadEdges finalSt)
          graph = G.buildG (0, n - 1) allEdges
          edgeLabelMap = Map.fromListWith (++)
            [ (e, [lbl]) | (e, lbl) <- bfsEdges finalSt ]
          payloadEdgeMap = Map.fromListWith (++)
            [ (e, [lbl]) | (e, lbl) <- bfsPayloadEdges finalSt ]
      in Right LocalGraph
           { lgGraph = graph
           , lgStart = 0
           , lgNodes = nodeArr
           , lgEdgeLabels = edgeLabelMap
           , lgPayloadEdges = payloadEdgeMap
           , lgStartVarHints = emptyRecVarHints
           }

bfsLoop :: SolveEnv -> BFSState -> BFSState
bfsLoop env st =
  case bfsQueue st of
    [] -> st
    (key : rest) ->
      let st' = st { bfsQueue = rest }
      in case classifyNode env key of
        Left err ->
          bfsLoop env st' { bfsErrors = err : bfsErrors st' }
        Right action ->
          let (node, successors) = actionToNode action
              thisVtx = bfsNodeMap st' Map.! key
              st'' = st' { bfsNodes = (thisVtx, node) : bfsNodes st' }
              (finalSt, _) = foldl'
                (processSuccessor key)
                (st'', ())
                successors
          in bfsLoop env finalSt

-- | Convert a NodeAction to a LocalNode and its outgoing successors.
actionToNode :: NodeAction -> (LocalNode, [Successor])
actionToNode NAEnd = (LocalEndNode, [])
actionToNode (NASend p labelMap) =
  let labels = Map.keys labelMap
      succs = [ BranchSucc l Send p reps | (l, reps) <- Map.toList labelMap ]
  in (LocalSendNode p labels, succs)
actionToNode (NARecv p labelMap) =
  let labels = Map.keys labelMap
      succs = [ BranchSucc l Receive p reps | (l, reps) <- Map.toList labelMap ]
  in (LocalRecvNode p labels, succs)
actionToNode (NAPayloadSend p pt reps) =
  (LocalPayloadSendNode p pt, [PayloadSucc Send p pt reps])
actionToNode (NAPayloadRecv p pt reps) =
  (LocalPayloadRecvNode p pt, [PayloadSucc Receive p pt reps])

-- | Process one successor edge during BFS.
processSuccessor :: NodeKey -> (BFSState, ()) -> Successor -> (BFSState, ())
processSuccessor srcKey (st, ()) succ_ =
  let srcVtx = bfsNodeMap st Map.! srcKey
      targetKey = succTargetKey succ_
  in case Map.lookup targetKey (bfsNodeMap st) of
    Just targetVtx ->
      let st' = addSuccEdge srcVtx targetVtx succ_ st
      in (st', ())
    Nothing ->
      let targetVtx = bfsNextVtx st
          st' = addSuccEdge srcVtx targetVtx succ_ st
      in (st' { bfsNodeMap = Map.insert targetKey targetVtx (bfsNodeMap st')
              , bfsNextVtx = targetVtx + 1
              , bfsQueue = bfsQueue st' ++ [targetKey]
              }, ())

succTargetKey :: Successor -> Set.Set ClassRep
succTargetKey (BranchSucc _ _ _ reps) = reps
succTargetKey (PayloadSucc _ _ _ reps) = reps

addSuccEdge :: G.Vertex -> G.Vertex -> Successor -> BFSState -> BFSState
addSuccEdge srcVtx targetVtx (BranchSucc lbl dir peer _) st =
  let edge = ((srcVtx, targetVtx), LocalEdgeLabel dir peer lbl emptyRecVarHints)
  in st { bfsEdges = edge : bfsEdges st }
addSuccEdge srcVtx targetVtx (PayloadSucc dir peer pt _) st =
  let edge = ((srcVtx, targetVtx), LocalPayloadEdgeLabel dir peer pt emptyRecVarHints)
  in st { bfsPayloadEdges = edge : bfsPayloadEdges st }

------------------------------------------------------------------------
-- Phase 3: Top-level
------------------------------------------------------------------------

infer :: Process -> Either [InferError] LocalType
infer proc =
  let initGenState = GenState { gsNextVar = 0, gsConstraints = [], gsErrors = [] }
      (rootVar, genSt) = runState (generate Map.empty proc) initGenState
  in case gsErrors genSt of
    errs@(_:_) -> Left errs
    [] ->
      let env = buildSolveEnv (gsConstraints genSt)
      in case solveConstraints env rootVar of
        Left errs -> Left errs
        Right graph ->
          case localGraphToType graph of
            Left err -> Left [InferGraphConversionError (show err)]
            Right lt -> Right (normalizeLocalBranchOrder lt)
