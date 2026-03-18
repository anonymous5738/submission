-- | Well-formedness checking for global and local session types.
module Syntax.WellFormed
  ( WFError(..)
  , checkGlobalType
  , checkLocalType
  , checkProcess
  , validateGlobalType
  , validateLocalType
  , validateProcess
  , parseGlobalTypeChecked
  , parseLocalTypeChecked
  , parseProcessChecked
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Env
import qualified Data.Set as Set
import Syntax.AST
import Syntax.Parser (parseGlobalType, parseLocalType, parseProcess)

-- | Errors produced by well-formedness validation.
data WFError
  = FreeTypeVar TypeVar
  | UnguardedTypeVar TypeVar
  | SelfCommunication Participant
  | DuplicateLabel Label
  | FreeProcessVar TypeVar
  | UnguardedProcessVar TypeVar
  | DuplicateProcessLabel Label
  deriving (Eq, Ord, Show)

-- | Validate a global type and return either all detected errors or success.
validateGlobalType :: GlobalType -> Either [WFError] ()
validateGlobalType g =
  case checkGlobalType g of
    [] -> Right ()
    es -> Left es

-- | Validate a local type and return either all detected errors or success.
validateLocalType :: LocalType -> Either [WFError] ()
validateLocalType t =
  case checkLocalType t of
    [] -> Right ()
    es -> Left es

-- | Parse and then validate a global type.
parseGlobalTypeChecked :: String -> Either String GlobalType
parseGlobalTypeChecked src = do
  g <- firstParse (parseGlobalType src)
  case validateGlobalType g of
    Left es -> Left (renderErrors es)
    Right _ -> Right g

-- | Parse and then validate a local type.
parseLocalTypeChecked :: String -> Either String LocalType
parseLocalTypeChecked src = do
  t <- firstParse (parseLocalType src)
  case validateLocalType t of
    Left es -> Left (renderErrors es)
    Right _ -> Right t

firstParse :: Show e => Either e b -> Either String b
firstParse = either (Left . show) Right

renderErrors :: [WFError] -> String
renderErrors = unlines . fmap show

-- | Collect all well-formedness errors for a global type.
checkGlobalType :: GlobalType -> [WFError]
checkGlobalType = checkGlobal Env.empty

checkGlobal :: Env.Map TypeVar Bool -> GlobalType -> [WFError]
checkGlobal env gtype =
  case gtype of
    GMessage sender receiver branches ->
      let dupErrs = duplicateLabelErrors (fmap fst (NE.toList branches))
          selfErr = [SelfCommunication sender | sender == receiver]
          env' = Env.map (const True) env
          branchErrs = concatMap (checkGlobal env' . snd) (NE.toList branches)
       in dupErrs ++ selfErr ++ branchErrs
    GVar v ->
      case Env.lookup v env of
        Nothing -> [FreeTypeVar v]
        Just guarded ->
          if guarded then [] else [UnguardedTypeVar v]
    GPayload sender receiver _ body ->
      [ SelfCommunication sender | sender == receiver ]
      ++ checkGlobal (Env.map (const True) env) body
    GRec v body ->
      checkGlobal (Env.insert v False env) body
    GEnd -> []

-- | Collect all well-formedness errors for a local type.
checkLocalType :: LocalType -> [WFError]
checkLocalType = checkLocal Env.empty

checkLocal :: Env.Map TypeVar Bool -> LocalType -> [WFError]
checkLocal env ltype =
  case ltype of
    LSend _ branches ->
      branchChecks branches env
    LRecv _ branches ->
      branchChecks branches env
    LPayloadSend _ _ cont ->
      checkLocal (Env.map (const True) env) cont
    LPayloadRecv _ _ cont ->
      checkLocal (Env.map (const True) env) cont
    LVar v ->
      case Env.lookup v env of
        Nothing -> [FreeTypeVar v]
        Just guarded ->
          if guarded then [] else [UnguardedTypeVar v]
    LRec v body ->
      checkLocal (Env.insert v False env) body
    LEnd -> []

branchChecks :: NE.NonEmpty (Label, LocalType) -> Env.Map TypeVar Bool -> [WFError]
branchChecks branches env =
  let dupErrs = duplicateLabelErrors (fmap fst (NE.toList branches))
      env' = Env.map (const True) env
      branchErrs = concatMap (\(_, t) -> checkLocal env' t) (NE.toList branches)
   in dupErrs ++ branchErrs

duplicateLabelErrors :: [Label] -> [WFError]
duplicateLabelErrors = go Set.empty
  where
    go _ [] = []
    go seen (l@(Label name) : xs)
      | name `Set.member` seen = DuplicateLabel l : go seen xs
      | otherwise = go (Set.insert name seen) xs

-- | Validate a process and return either all detected errors or success.
validateProcess :: Process -> Either [WFError] ()
validateProcess p =
  case checkProcess p of
    [] -> Right ()
    es -> Left es

-- | Parse and then validate a process.
parseProcessChecked :: String -> Either String Process
parseProcessChecked src = do
  p <- firstParse (parseProcess src)
  case validateProcess p of
    Left es -> Left (renderErrors es)
    Right _ -> Right p

-- | Collect all well-formedness errors for a process.
checkProcess :: Process -> [WFError]
checkProcess = checkProc Env.empty

checkProc :: Env.Map TypeVar Bool -> Process -> [WFError]
checkProc env proc =
  case proc of
    PSend _ _ cont ->
      let env' = Env.map (const True) env
      in checkProc env' cont
    PRecv _ branches ->
      let dupErrs = duplicateProcessLabelErrors (fmap fst (NE.toList branches))
          env' = Env.map (const True) env
          branchErrs = concatMap (checkProc env' . snd) (NE.toList branches)
      in dupErrs ++ branchErrs
    PSendPayload _ _ cont ->
      checkProc (Env.map (const True) env) cont
    PRecvPayload _ _ cont ->
      checkProc (Env.map (const True) env) cont
    PIf _ p q ->
      checkProc env p ++ checkProc env q
    PVar v ->
      case Env.lookup v env of
        Nothing -> [FreeProcessVar v]
        Just guarded ->
          if guarded then [] else [UnguardedProcessVar v]
    PRec v body ->
      checkProc (Env.insert v False env) body
    PEnd -> []

duplicateProcessLabelErrors :: [Label] -> [WFError]
duplicateProcessLabelErrors = go Set.empty
  where
    go _ [] = []
    go seen (l@(Label name) : xs)
      | name `Set.member` seen = DuplicateProcessLabel l : go seen xs
      | otherwise = go (Set.insert name seen) xs
