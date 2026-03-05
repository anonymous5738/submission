-- | Typechecking processes against local session types.
--
-- Verifies that a 'Process' correctly implements a 'LocalType' protocol
-- using standard session typing rules (T-Inact, T-Rec, T-Var, T-Sel, T-Bra, T-Cond, T-Sub).
module Typecheck
  ( ExprType(..)
  , TypeError(..)
  , typecheck
  , inferExprType
  ) where

import Automata (buildLocalGraph)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Subtyping (checkLocalSubtype)
import Syntax.AST

-- | Simple expression types for partial inference.
data ExprType = TInt | TBool | TAny
  deriving (Eq, Show)

-- | Errors produced by the typechecker.
data TypeError
  = ParticipantMismatch Participant Participant
  | LabelNotInType Label [Label]
  | MissingProcessBranch Label [Label]
  | StructuralMismatch Process LocalType
  | UnboundProcessVar TypeVar
  | SubtypingFailed TypeVar LocalType LocalType
  | ConditionNotBool Expr ExprType
  | ExprTypeMismatch ExprType ExprType Expr
  deriving (Eq, Show)

-- | Check that a process implements a local type protocol.
typecheck :: Process -> LocalType -> Either [TypeError] ()
typecheck proc ltype = check Map.empty proc ltype

type Env = Map.Map TypeVar LocalType

check :: Env -> Process -> LocalType -> Either [TypeError] ()
check env proc ltype =
  case proc of
    PEnd ->
      let t = unfoldRec ltype
       in case t of
            LEnd -> Right ()
            _    -> Left [StructuralMismatch proc ltype]

    PVar v ->
      case Map.lookup v env of
        Nothing -> Left [UnboundProcessVar v]
        Just boundType -> subtypeCheck v boundType ltype

    PRec v body ->
      check (Map.insert v ltype env) body ltype

    PSend p l cont ->
      let t = unfoldRec ltype
       in case t of
            LSend p' branches
              | p /= p' -> Left [ParticipantMismatch p p']
              | otherwise ->
                  case lookup l (NE.toList branches) of
                    Nothing -> Left [LabelNotInType l (map fst (NE.toList branches))]
                    Just contType -> check env cont contType
            _ -> Left [StructuralMismatch proc ltype]

    PRecv p branches ->
      let t = unfoldRec ltype
       in case t of
            LRecv p' typeBranches
              | p /= p' -> Left [ParticipantMismatch p p']
              | otherwise ->
                  let typeLabels = map fst (NE.toList typeBranches)
                      procLabels = map fst (NE.toList branches)
                      missingLabels =
                        [ MissingProcessBranch tl procLabels
                        | tl <- typeLabels
                        , tl `notElem` procLabels
                        ]
                   in case missingLabels of
                        (_:_) -> Left missingLabels
                        [] ->
                          let typeBranchMap = Map.fromList (NE.toList typeBranches)
                              results =
                                [ check env cont contType
                                | (lbl, cont) <- NE.toList branches
                                , Just contType <- [Map.lookup lbl typeBranchMap]
                                ]
                              errors = concatMap (\r -> case r of Left es -> es; Right () -> []) results
                           in if null errors then Right () else Left errors
            _ -> Left [StructuralMismatch proc ltype]

    PIf e thenP elseP ->
      let t = unfoldRec ltype
          condErrors = case inferExprType e of
            Left err -> [err]
            Right ety ->
              if checkTypeCompat TBool ety
                then []
                else [ConditionNotBool e ety]
          thenResult = check env thenP t
          elseResult = check env elseP t
          branchErrors =
            (case thenResult of Left es -> es; Right () -> [])
            ++ (case elseResult of Left es -> es; Right () -> [])
       in case condErrors ++ branchErrors of
            [] -> Right ()
            errs -> Left errs

-- | Check subtyping at PVar: equality fast-path, then automata-based check.
subtypeCheck :: TypeVar -> LocalType -> LocalType -> Either [TypeError] ()
subtypeCheck var boundType expectedType
  | boundType == expectedType = Right ()
  | otherwise =
      case checkLocalSubtype (buildLocalGraph boundType) (buildLocalGraph expectedType) of
        Right () -> Right ()
        Left _   -> Left [SubtypingFailed var boundType expectedType]

-- | Infer the type of an expression.
inferExprType :: Expr -> Either TypeError ExprType
inferExprType expr =
  case expr of
    EInt _  -> Right TInt
    EBool _ -> Right TBool
    EVar _  -> Right TAny
    ENot e  -> do
      t <- inferExprType e
      expectType TBool t e
      Right TBool
    EBinOp op l r -> inferBinOp op l r

inferBinOp :: BinOp -> Expr -> Expr -> Either TypeError ExprType
inferBinOp op l r = do
  lt <- inferExprType l
  rt <- inferExprType r
  case op of
    Add -> expectType TInt lt l >> expectType TInt rt r >> Right TInt
    Sub -> expectType TInt lt l >> expectType TInt rt r >> Right TInt
    Mul -> expectType TInt lt l >> expectType TInt rt r >> Right TInt
    Lt  -> expectType TInt lt l >> expectType TInt rt r >> Right TBool
    Gt  -> expectType TInt lt l >> expectType TInt rt r >> Right TBool
    Eq  -> expectType TInt lt l >> expectType TInt rt r >> Right TBool
    Neq -> expectType TInt lt l >> expectType TInt rt r >> Right TBool
    And -> expectType TBool lt l >> expectType TBool rt r >> Right TBool
    Or  -> expectType TBool lt l >> expectType TBool rt r >> Right TBool

expectType :: ExprType -> ExprType -> Expr -> Either TypeError ()
expectType expected actual e
  | checkTypeCompat expected actual = Right ()
  | otherwise = Left (ExprTypeMismatch expected actual e)

-- | TAny is compatible with any expected type.
checkTypeCompat :: ExprType -> ExprType -> Bool
checkTypeCompat _ TAny = True
checkTypeCompat expected actual = expected == actual
