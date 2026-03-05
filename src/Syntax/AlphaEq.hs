-- | Alpha-equivalence and normalization helpers for session-type syntax trees.
module Syntax.AlphaEq
  ( alphaEqGlobalType
  , alphaEqLocalType
  , normalizeGlobalBranchOrder
  , normalizeLocalBranchOrder
  ) where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Syntax.AST

-- | Alpha-equivalence for global types.
alphaEqGlobalType :: GlobalType -> GlobalType -> Bool
alphaEqGlobalType = goGlobal Map.empty
  where
    goGlobal env g1 g2 =
      case (g1, g2) of
        (GEnd, GEnd) -> True
        (GVar v1, GVar v2) ->
          case Map.lookup v1 env of
            Just expectedV2 -> v2 == expectedV2
            Nothing -> v1 == v2
        (GRec v1 b1, GRec v2 b2) ->
          goGlobal (Map.insert v1 v2 env) b1 b2
        (GMessage s1 r1 bs1, GMessage s2 r2 bs2) ->
          s1 == s2
            && r1 == r2
            && sameLabels
            && and
              [ goGlobal env t1 t2
              | ((_, t1), (_, t2)) <- zip l1 l2
              ]
          where
            l1 = NE.toList bs1
            l2 = NE.toList bs2
            sameLabels = fmap fst l1 == fmap fst l2
        _ -> False

-- | Alpha-equivalence for local types.
alphaEqLocalType :: LocalType -> LocalType -> Bool
alphaEqLocalType = goLocal Map.empty
  where
    goLocal env t1 t2 =
      case (t1, t2) of
        (LEnd, LEnd) -> True
        (LVar v1, LVar v2) ->
          case Map.lookup v1 env of
            Just expectedV2 -> v2 == expectedV2
            Nothing -> v1 == v2
        (LRec v1 b1, LRec v2 b2) ->
          goLocal (Map.insert v1 v2 env) b1 b2
        (LSend p1 bs1, LSend p2 bs2) ->
          sameBranches p1 p2 bs1 bs2
        (LRecv p1 bs1, LRecv p2 bs2) ->
          sameBranches p1 p2 bs1 bs2
        _ -> False
      where
        sameBranches p1 p2 bs1 bs2 =
          p1 == p2
            && sameLabels
            && and
              [ goLocal env c1 c2
              | ((_, c1), (_, c2)) <- zip l1 l2
              ]
          where
            l1 = NE.toList bs1
            l2 = NE.toList bs2
            sameLabels = fmap fst l1 == fmap fst l2

-- | Normalize global types by recursively sorting branch lists by label.
normalizeGlobalBranchOrder :: GlobalType -> GlobalType
normalizeGlobalBranchOrder g =
  case g of
    GMessage s r bs ->
      let sorted = sortOn fst (fmap (\(l, t) -> (l, normalizeGlobalBranchOrder t)) (NE.toList bs))
       in case sorted of
            [] -> error "Syntax.AlphaEq: impossible empty global branch list"
            b0 : rest -> GMessage s r (b0 NE.:| rest)
    GRec v body -> GRec v (normalizeGlobalBranchOrder body)
    GVar v -> GVar v
    GEnd -> GEnd

-- | Normalize local types by recursively sorting branch lists by label.
normalizeLocalBranchOrder :: LocalType -> LocalType
normalizeLocalBranchOrder t =
  case t of
    LSend p bs ->
      let sorted = sortOn fst (fmap (\(l, c) -> (l, normalizeLocalBranchOrder c)) (NE.toList bs))
       in case sorted of
            [] -> error "Syntax.AlphaEq: impossible empty local send branch list"
            b0 : rest -> LSend p (b0 NE.:| rest)
    LRecv p bs ->
      let sorted = sortOn fst (fmap (\(l, c) -> (l, normalizeLocalBranchOrder c)) (NE.toList bs))
       in case sorted of
            [] -> error "Syntax.AlphaEq: impossible empty local recv branch list"
            b0 : rest -> LRecv p (b0 NE.:| rest)
    LRec v body -> LRec v (normalizeLocalBranchOrder body)
    LVar v -> LVar v
    LEnd -> LEnd
