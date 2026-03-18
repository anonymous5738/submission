module Benchmark.Types
  ( Citation(..)
  , GlobalExample(..)
  , LocalExample(..)
  , ParsedGlobalExample(..)
  , ParsedLocalExample(..)
  , parseGlobalExample
  , parseLocalExample
  ) where

import qualified Data.Map.Strict as Map
import Syntax.AST (GlobalType, LocalType, Participant(..))
import Syntax.WellFormed (parseGlobalTypeChecked, parseLocalTypeChecked)

data Citation = Citation
  { citeKey   :: String
  , citeShort :: String
  , citeRef   :: Maybe String  -- ^ Example/figure reference, e.g. "Ex.~4.8"
  }

data GlobalExample = GlobalExample
  { geName             :: String
  , geDisplayName      :: String   -- ^ Human-readable name for tables
  , geCitation         :: Maybe Citation
  , geGlobalSource     :: String
  , geParticipantNames :: [String]
  }

data LocalExample = LocalExample
  { leName         :: String
  , leDisplayName  :: String   -- ^ Human-readable name for tables
  , leCitation     :: Maybe Citation
  , leParticipants :: [(String, String)]  -- ^ (participant name, local type source)
  }

data ParsedGlobalExample = ParsedGlobalExample
  { pgeName         :: String
  , pgeDisplayName  :: String
  , pgeCitation     :: Maybe Citation
  , pgeGlobalType   :: GlobalType
  , pgeParticipants :: [Participant]
  }

data ParsedLocalExample = ParsedLocalExample
  { pleName             :: String
  , pleDisplayName      :: String
  , pleCitation         :: Maybe Citation
  , pleContext          :: Map.Map Participant LocalType
  , pleParticipantOrder :: [Participant]
  }

parseGlobalExample :: GlobalExample -> Either String ParsedGlobalExample
parseGlobalExample ex = do
  gt <- case parseGlobalTypeChecked (geGlobalSource ex) of
    Left err -> Left ("Global type parse error in " ++ geName ex ++ ": " ++ err)
    Right g  -> Right g
  Right ParsedGlobalExample
    { pgeName        = geName ex
    , pgeDisplayName = geDisplayName ex
    , pgeCitation    = geCitation ex
    , pgeGlobalType  = gt
    , pgeParticipants = map Participant (geParticipantNames ex)
    }

parseLocalExample :: LocalExample -> Either String ParsedLocalExample
parseLocalExample ex = do
  locals <- mapM parseOne (leParticipants ex)
  Right ParsedLocalExample
    { pleName             = leName ex
    , pleDisplayName      = leDisplayName ex
    , pleCitation         = leCitation ex
    , pleContext          = Map.fromList [(Participant n, lt) | (n, lt) <- locals]
    , pleParticipantOrder = map (Participant . fst) (leParticipants ex)
    }
  where
    parseOne (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left ("Parse error for " ++ name ++ " in " ++ leName ex ++ ": " ++ err)
        Right lt -> Right (name, lt)
