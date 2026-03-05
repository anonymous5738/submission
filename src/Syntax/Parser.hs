-- | Megaparsec parsers for global and local session types.
module Syntax.Parser
  ( Parser
  , parseGlobalType
  , parseLocalType
  , parseProcess
  , globalTypeParser
  , localTypeParser
  , processParser
  , exprParser
  ) where

import Control.Applicative (empty, many, (<|>))
import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import Syntax.AST
import Text.Megaparsec
  ( Parsec
  , ParseErrorBundle
  , between
  , choice
  , eof
  , label
  , notFollowedBy
  , sepBy1
  , try
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type used across syntax modules.
type Parser = Parsec Void String

-- | Parse a full global type expression.
parseGlobalType :: String -> Either (ParseErrorBundle String Void) GlobalType
parseGlobalType = MP.parse (sc *> globalTypeParser <* eof) "global type"

-- | Parse a full local type expression.
parseLocalType :: String -> Either (ParseErrorBundle String Void) LocalType
parseLocalType = MP.parse (sc *> localTypeParser <* eof) "local type"

-- | Parse a full process expression.
parseProcess :: String -> Either (ParseErrorBundle String Void) Process
parseProcess = MP.parse (sc *> processParser <* eof) "process"

-- | Parser for /global/ type terms.
globalTypeParser :: Parser GlobalType
globalTypeParser =
  label "global type" . choice $
    [ gRec
    , GEnd <$ keyword "end"
    , try gMessage
    , GVar <$> typeVarP
    , parens globalTypeParser
    ]
  where
    gRec = GRec <$> (keyword "rec" *> typeVarP) <*> (symbol "." *> globalTypeParser)
    gMessage = do
      sender <- participantP
      _ <- symbol "->"
      receiver <- participantP
      branches <- branchBlock globalTypeParser
      pure (GMessage sender receiver branches)

-- | Parser for /local/ type terms.
localTypeParser :: Parser LocalType
localTypeParser =
  label "local type" . choice $
    [ lRec
    , LEnd <$ keyword "end"
    , try send
    , try recv
    , LVar <$> typeVarP
    , parens localTypeParser
    ]
  where
    lRec = LRec <$> (keyword "rec" *> typeVarP) <*> (symbol "." *> localTypeParser)
    send = LSend <$> participantP <*> (symbol "!" *> branchBlock localTypeParser)
    recv = LRecv <$> participantP <*> (symbol "?" *> branchBlock localTypeParser)

-- | Parser for /process/ terms.
processParser :: Parser Process
processParser =
  label "process" . choice $
    [ pRec
    , pIf
    , try pSend
    , try pRecv
    , pEnd
    , pVar
    , parens processParser
    ]
  where
    pRec  = PRec <$ keyword "rec" <*> typeVarP <* symbol "." <*> processParser
    pIf   = PIf <$ keyword "if" <*> exprParser <* keyword "then" <*> processParser <* keyword "else" <*> processParser
    pSend = PSend <$> participantP <* symbol "!" <*> labelP <* symbol "." <*> processParser
    pRecv = PRecv <$> participantP <* symbol "?" <*> branchBlock processParser
    pEnd  = PEnd <$ symbol "0"
    pVar  = PVar <$> typeVarP

-- | Parser for expressions in process terms.
exprParser :: Parser Expr
exprParser = makeExprParser exprAtom operatorTable
  where
    exprAtom :: Parser Expr
    exprAtom = choice
      [ EBool True <$ keyword "true"
      , EBool False <$ keyword "false"
      , EInt <$> lexeme L.decimal
      , EVar <$> identifier
      , parens exprParser
      ]

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [ Prefix (ENot <$ keyword "not") ]
      , [ InfixL (EBinOp Mul <$ symbol "*") ]
      , [ InfixL (EBinOp Add <$ symbol "+")
        , InfixL (EBinOp Sub <$ symbol "-")
        ]
      , [ InfixN (EBinOp Lt <$ symbol "<")
        , InfixN (EBinOp Gt <$ symbol ">")
        ]
      , [ InfixN (EBinOp Eq <$ symbol "==")
        , InfixN (EBinOp Neq <$ symbol "!=")
        ]
      , [ InfixL (EBinOp And <$ symbol "&&") ]
      , [ InfixL (EBinOp Or <$ symbol "||") ]
      ]

branchBlock :: Parser a -> Parser (Branches a)
branchBlock parser =
  NE.fromList <$> between (symbol "{") (symbol "}") (sepBy1 (branch parser) (symbol ","))

branch :: Parser a -> Parser (Label, a)
branch parser = do
  lbl <- labelP
  _ <- symbol ":"
  t <- parser
  pure (lbl, t)

participantP :: Parser Participant
participantP = Participant <$> identifier

labelP :: Parser Label
labelP = Label <$> identifier

typeVarP :: Parser TypeVar
typeVarP = TypeVar <$> identifier

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme . try $ do
  name <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '\'')
  when (name `elem` keywords) $
    fail ("reserved word " <> show name <> " cannot be used here")
  pure name

keyword :: String -> Parser String
keyword w = lexeme . try $ string w <* notFollowedBy identChar

identChar :: Parser Char
identChar = alphaNumChar <|> char '_' <|> char '\''

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 empty empty

keywords :: [String]
keywords = ["rec", "end", "if", "then", "else", "true", "false", "not"]
