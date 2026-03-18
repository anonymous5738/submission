-- | Pretty-printers for global and local session types.
module Syntax.Pretty
  ( prettyGlobalType
  , prettyLocalType
  , prettyProcess
  , prettyExpr
  , renderGlobalType
  , renderLocalType
  , renderProcess
  ) where

import qualified Data.List.NonEmpty as NE
import Prettyprinter
  ( Doc
  , brackets
  , colon
  , comma
  , defaultLayoutOptions
  , encloseSep
  , hsep
  , layoutPretty
  , parens
  , pretty
  , semi
  , space
  , (<+>)
  )
import Prettyprinter.Render.String (renderString)
import Syntax.AST

-- | Pretty-print a global type as a document.
prettyGlobalType :: GlobalType -> Doc ann
prettyGlobalType (GMessage p q bs) =
  hsep [prettyParticipant p, pretty "->", prettyParticipant q, prettyBranches prettyGlobalType bs]
prettyGlobalType (GVar v) = prettyTypeVar v
prettyGlobalType (GRec v g) = hsep [pretty "rec", prettyTypeVar v, pretty ".", prettyGlobalType g]
prettyGlobalType (GPayload p q pt cont) =
  hsep [prettyParticipant p, pretty "->", prettyParticipant q,
        brackets (prettyPayloadType pt) <> semi, prettyGlobalType cont]
prettyGlobalType GEnd = pretty "end"

-- | Pretty-print a local type as a document.
prettyLocalType :: LocalType -> Doc ann
prettyLocalType (LSend p bs) = hsep [prettyParticipant p, pretty "!", prettyBranches prettyLocalType bs]
prettyLocalType (LRecv p bs) = hsep [prettyParticipant p, pretty "?", prettyBranches prettyLocalType bs]
prettyLocalType (LPayloadSend p pt cont) =
  hsep [prettyParticipant p, pretty "!" <> brackets (prettyPayloadType pt) <> semi, prettyLocalType cont]
prettyLocalType (LPayloadRecv p pt cont) =
  hsep [prettyParticipant p, pretty "?" <> brackets (prettyPayloadType pt) <> semi, prettyLocalType cont]
prettyLocalType (LVar v) = prettyTypeVar v
prettyLocalType (LRec v t) = hsep [pretty "rec", prettyTypeVar v, pretty ".", prettyLocalType t]
prettyLocalType LEnd = pretty "end"

-- | Pretty-print a process term as a document.
prettyProcess :: Process -> Doc ann
prettyProcess (PSend p l cont) =
  hsep [prettyParticipant p, pretty "!", prettyLabel l, pretty ".", prettyProcess cont]
prettyProcess (PRecv p bs) =
  hsep [prettyParticipant p, pretty "?", prettyBranches prettyProcess bs]
prettyProcess (PSendPayload p e cont) =
  hsep [prettyParticipant p, pretty "!" <> brackets (prettyExpr e), pretty ".", prettyProcess cont]
prettyProcess (PRecvPayload p var cont) =
  hsep [prettyParticipant p, pretty "?" <> parens (pretty var), pretty ".", prettyProcess cont]
prettyProcess (PIf e p q) =
  hsep [pretty "if", prettyExpr e, pretty "then", prettyProcess p, pretty "else", prettyProcess q]
prettyProcess (PRec v p) =
  hsep [pretty "rec", prettyTypeVar v, pretty ".", prettyProcess p]
prettyProcess (PVar v) = prettyTypeVar v
prettyProcess PEnd = pretty "0"

-- | Pretty-print an expression (top-level, no outer parens).
prettyExpr :: Expr -> Doc ann
prettyExpr = prettyExprPrec 0

-- | Precedence-aware expression pretty-printing.
prettyExprPrec :: Int -> Expr -> Doc ann
prettyExprPrec _ (EInt n) = pretty n
prettyExprPrec _ (EBool True) = pretty "true"
prettyExprPrec _ (EBool False) = pretty "false"
prettyExprPrec _ (EVar x) = pretty x
prettyExprPrec _ EUnit = pretty "()"
prettyExprPrec prec (ENot e) =
  parensWhen (prec > 6) $ hsep [pretty "not", prettyExprPrec 7 e]
prettyExprPrec prec (EBinOp op l r) =
  let (opPrec, opStr) = binOpInfo op
  in parensWhen (prec > opPrec) $
       hsep [prettyExprPrec (opPrec + 1) l, pretty opStr, prettyExprPrec (opPrec + 1) r]

-- | Get precedence level and string representation for a binary operator.
binOpInfo :: BinOp -> (Int, String)
binOpInfo Or  = (0, "||")
binOpInfo And = (1, "&&")
binOpInfo Eq  = (2, "==")
binOpInfo Neq = (2, "!=")
binOpInfo Lt  = (3, "<")
binOpInfo Gt  = (3, ">")
binOpInfo Add = (4, "+")
binOpInfo Sub = (4, "-")
binOpInfo Mul = (5, "*")

-- | Wrap in parentheses if the condition is true.
parensWhen :: Bool -> Doc ann -> Doc ann
parensWhen True  d = pretty "(" <> d <> pretty ")"
parensWhen False d = d

-- | Render a global type to a 'String'.
renderGlobalType :: GlobalType -> String
renderGlobalType = renderDoc . prettyGlobalType

-- | Render a local type to a 'String'.
renderLocalType :: LocalType -> String
renderLocalType = renderDoc . prettyLocalType

-- | Render a process to a 'String'.
renderProcess :: Process -> String
renderProcess = renderDoc . prettyProcess

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty defaultLayoutOptions

prettyParticipant :: Participant -> Doc ann
prettyParticipant (Participant p) = pretty p

prettyLabel :: Label -> Doc ann
prettyLabel (Label l) = pretty l

prettyTypeVar :: TypeVar -> Doc ann
prettyTypeVar (TypeVar t) = pretty t

prettyPayloadType :: PayloadType -> Doc ann
prettyPayloadType PTInt    = pretty "int"
prettyPayloadType PTBool   = pretty "bool"
prettyPayloadType PTUnit   = pretty "unit"
prettyPayloadType PTString = pretty "string"
prettyPayloadType PTFloat  = pretty "float"

prettyBranches :: (a -> Doc ann) -> Branches a -> Doc ann
prettyBranches renderElem branches =
  encloseSep (pretty "{") (pretty "}") (comma <> space) (branchDoc <$> NE.toList branches)
  where
    branchDoc (lbl, t) = prettyLabel lbl <> colon <+> renderElem t
