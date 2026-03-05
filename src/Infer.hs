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
                 LocalEdgeLabel(..), emptyRecVarHints,
                 localGraphToType)
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
  deriving (Eq, Show)

data StructBound
  = BEnd
  | BSend Participant Label InferVar
  | BRecv Participant [(Label, InferVar)]

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
  let ends  = [() | CBEnd <- bounds]
      sends = [(p, l, v) | CBSend p l v <- bounds]
      recvs = [(p, branches) | CBRecv p branches <- bounds]
  in case (ends, sends, recvs) of
    -- All end
    (_:_, [], []) -> Right NAEnd
    -- All sends
    ([], _:_, []) ->
      let participants = map (\(p,_,_) -> p) sends
          p0 = head participants
      in if all (== p0) participants
         then
           let labelMap = foldl'
                 (\m (_, l, v) -> Map.insertWith Set.union l (Set.singleton v) m)
                 Map.empty sends
           in Right (NASend p0 labelMap)
         else Left (InferParticipantMismatch p0 (head (filter (/= p0) participants)))
    -- All recvs
    ([], [], _:_) ->
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
    -- Any mixture of end/send/recv is an error
    _ -> Left InferActionMismatch

-- | BFS state for building the graph.
data BFSState = BFSState
  { bfsNodeMap   :: Map.Map NodeKey G.Vertex
  , bfsNextVtx   :: G.Vertex
  , bfsNodes     :: [(G.Vertex, LocalNode)]
  , bfsEdges     :: [(G.Edge, LocalEdgeLabel)]
  , bfsQueue     :: [NodeKey]
  , bfsErrors    :: [InferError]
  }

-- | Build a 'LocalGraph' from constraints via BFS over class-rep sets.
solveConstraints :: SolveEnv -> InferVar -> Either [InferError] LocalGraph
solveConstraints env rootVar =
  let rootKey = Set.singleton (canonVar env rootVar)
      initState = BFSState
        { bfsNodeMap = Map.singleton rootKey 0
        , bfsNextVtx = 1
        , bfsNodes = []
        , bfsEdges = []
        , bfsQueue = [rootKey]
        , bfsErrors = []
        }
      finalSt = bfsLoop env initState
  in case bfsErrors finalSt of
    errs@(_:_) -> Left errs
    [] ->
      let n = bfsNextVtx finalSt
          nodeArr = array (0, n - 1) (bfsNodes finalSt)
          edgeList = map fst (bfsEdges finalSt)
          graph = G.buildG (0, n - 1) edgeList
          edgeLabelMap = Map.fromListWith (++)
            [ (e, [lbl]) | (e, lbl) <- bfsEdges finalSt ]
      in Right LocalGraph
           { lgGraph = graph
           , lgStart = 0
           , lgNodes = nodeArr
           , lgEdgeLabels = edgeLabelMap
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
actionToNode :: NodeAction
             -> (LocalNode, [(Label, LocalDirection, Participant, Set.Set ClassRep)])
actionToNode NAEnd = (LocalEndNode, [])
actionToNode (NASend p labelMap) =
  let labels = Map.keys labelMap
      succs = [ (l, Send, p, reps) | (l, reps) <- Map.toList labelMap ]
  in (LocalSendNode p labels, succs)
actionToNode (NARecv p labelMap) =
  let labels = Map.keys labelMap
      succs = [ (l, Receive, p, reps) | (l, reps) <- Map.toList labelMap ]
  in (LocalRecvNode p labels, succs)

-- | Process one successor edge during BFS.
processSuccessor :: NodeKey
                 -> (BFSState, ())
                 -> (Label, LocalDirection, Participant, Set.Set ClassRep)
                 -> (BFSState, ())
processSuccessor srcKey (st, ()) (lbl, dir, peer, targetKey) =
  let srcVtx = bfsNodeMap st Map.! srcKey
  in case Map.lookup targetKey (bfsNodeMap st) of
    Just targetVtx ->
      let edge = ((srcVtx, targetVtx), LocalEdgeLabel dir peer lbl emptyRecVarHints)
      in (st { bfsEdges = edge : bfsEdges st }, ())
    Nothing ->
      let targetVtx = bfsNextVtx st
          edge = ((srcVtx, targetVtx), LocalEdgeLabel dir peer lbl emptyRecVarHints)
      in (st { bfsNodeMap = Map.insert targetKey targetVtx (bfsNodeMap st)
             , bfsNextVtx = targetVtx + 1
             , bfsEdges = edge : bfsEdges st
             , bfsQueue = bfsQueue st ++ [targetKey]
             }, ())

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
