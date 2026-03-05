{-# LANGUAGE DeriveGeneric #-}

-- | Core syntax tree definitions for global and local session types.
module Syntax.AST
  ( Participant(..)
  , Label(..)
  , TypeVar(..)
  , Branches
  , GlobalType(..)
  , LocalType(..)
  , Expr(..)
  , BinOp(..)
  , Process(..)
  , substituteVar
  , unfoldRec
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

-- | Participant identifiers in a protocol.
newtype Participant = Participant { getParticipant :: String }
  deriving (Eq, Ord, Show, Generic)

instance NFData Participant

-- | Message labels used to distinguish branches.
newtype Label = Label { getLabel :: String }
  deriving (Eq, Ord, Show, Generic)

instance NFData Label

-- | Type variables used for recursive types.
newtype TypeVar = TypeVar { getTypeVar :: String }
  deriving (Eq, Ord, Show, Generic)

instance NFData TypeVar

-- | A non-empty set of labelled continuations.
type Branches t = NonEmpty (Label, t)

-- | Global types describe whole-protocol behaviour.
data GlobalType
  = GMessage Participant Participant (Branches GlobalType) -- ^ p -> q {l1: G1, ..., ln: Gn}
  | GVar TypeVar                                           -- ^ Type variable t
  | GRec TypeVar GlobalType                                -- ^ rec t . G
  | GEnd                                                   -- ^ end
  deriving (Eq, Show, Generic)

instance NFData GlobalType

-- | Local types describe a single participant's behaviour.
data LocalType
  = LSend Participant (Branches LocalType) -- ^ Internal choice: p ! {l1: T1, ..., ln: Tn}
  | LRecv Participant (Branches LocalType) -- ^ External choice: p ? {l1: T1, ..., ln: Tn}
  | LVar TypeVar                           -- ^ Type variable t
  | LRec TypeVar LocalType                 -- ^ rec t . T
  | LEnd                                   -- ^ end
  deriving (Eq, Show, Generic)

instance NFData LocalType

-- | Expressions in process terms.
data Expr
  = EInt Int                     -- ^ Integer literal
  | EBool Bool                   -- ^ Boolean literal
  | EVar String                  -- ^ Expression variable
  | EBinOp BinOp Expr Expr       -- ^ Binary operation
  | ENot Expr                    -- ^ Logical negation
  deriving (Eq, Show, Generic)

instance NFData Expr

-- | Binary operators for expressions.
data BinOp = Add | Sub | Mul | Lt | Gt | Eq | Neq | And | Or
  deriving (Eq, Ord, Show, Generic)

instance NFData BinOp

-- | Process terms that implement a local type protocol.
data Process
  = PSend Participant Label Process        -- ^ p ! l . P
  | PRecv Participant (Branches Process)   -- ^ p ? { l1: P1, ..., ln: Pn }
  | PIf Expr Process Process               -- ^ if e then P else P
  | PVar TypeVar                           -- ^ Process variable X
  | PRec TypeVar Process                   -- ^ rec X . P
  | PEnd                                   -- ^ Terminated process (0)
  deriving (Eq, Show, Generic)

instance NFData Process

-- | Substitute free occurrences of a type variable with a replacement in a local type.
-- Respects shadowing: if a nested 'LRec' binds the same variable, substitution stops.
substituteVar :: TypeVar -> LocalType -> LocalType -> LocalType
substituteVar target replacement = go
  where
    go localType =
      case localType of
        LSend peer branches ->
          LSend peer (fmap (\(lbl, cont) -> (lbl, go cont)) branches)
        LRecv peer branches ->
          LRecv peer (fmap (\(lbl, cont) -> (lbl, go cont)) branches)
        LVar tv
          | tv == target -> replacement
          | otherwise -> LVar tv
        LRec tv body
          | tv == target -> LRec tv body
          | otherwise -> LRec tv (go body)
        LEnd -> LEnd

-- | Unfold a recursive local type by one step.
-- @unfoldRec (LRec v body) = body[v := LRec v body]@; anything else is returned unchanged.
unfoldRec :: LocalType -> LocalType
unfoldRec localType =
  case localType of
    LRec tv body -> substituteVar tv localType body
    _ -> localType
