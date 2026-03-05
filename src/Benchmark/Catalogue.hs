module Benchmark.Catalogue
  ( BenchExample(..)
  , Citation(..)
  , ParsedExample(..)
  , benchmarkExamples
  , parseBenchExample
  ) where

import qualified Data.Map.Strict as Map
import Syntax.AST (GlobalType, LocalType, Participant(..))
import Syntax.WellFormed (parseGlobalTypeChecked, parseLocalTypeChecked)

data Citation = Citation
  { citeKey   :: String
  , citeShort :: String
  }

data BenchExample = BenchExample
  { beName         :: String
  , beCitation     :: Maybe Citation
  , beParticipants :: [(String, String)]
  , beGlobalSource :: Maybe String
  }

data ParsedExample = ParsedExample
  { peName             :: String
  , peCitation         :: Maybe Citation
  , peContext          :: Map.Map Participant LocalType
  , peGlobalType       :: Maybe GlobalType
  , peParticipantOrder :: [Participant]
  }

parseBenchExample :: BenchExample -> Either String ParsedExample
parseBenchExample ex = do
  locals <- mapM parseOne (beParticipants ex)
  globalType <- case beGlobalSource ex of
    Nothing  -> Right Nothing
    Just src -> case parseGlobalTypeChecked src of
      Left err -> Left ("Global type parse error in " ++ beName ex ++ ": " ++ err)
      Right gt -> Right (Just gt)
  let participants = map (Participant . fst) (beParticipants ex)
      ctx = Map.fromList [(Participant name, lt) | ((name, _), lt) <- zip (beParticipants ex) (map snd locals)]
  Right ParsedExample
    { peName             = beName ex
    , peCitation         = beCitation ex
    , peContext          = ctx
    , peGlobalType       = globalType
    , peParticipantOrder = participants
    }
  where
    parseOne (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left ("Local type parse error for " ++ name ++ " in " ++ beName ex ++ ": " ++ err)
        Right lt -> Right (name, lt)

-- ---------------------------------------------------------------------------
-- Citations
-- ---------------------------------------------------------------------------

scalasYoshida2019 :: Citation
scalasYoshida2019 = Citation "scalas2019less" "Scalas \\& Yoshida, 2019"

udomYoshida2025 :: Citation
udomYoshida2025 = Citation "udom2025topdown" "Udomsrirungruang \\& Yoshida, 2025"

tiroreEtAl2023 :: Citation
tiroreEtAl2023 = Citation "tirore2023sound" "Tirore et al., 2023"

castroPerezEtAl2026 :: Citation
castroPerezEtAl2026 = Citation "castroperez2026synthetic" "Castro-Perez et al., 2026"

-- ---------------------------------------------------------------------------
-- Benchmark examples
-- ---------------------------------------------------------------------------

benchmarkExamples :: [BenchExample]
benchmarkExamples =
  [ exTwoBuyers
  , exMapReduce5
  , exOAuth
  , exGIp
  , exGIf
  , exGItp23
  , exGRing
  , exDelta5
  , exDelta6
  , exDelta7
  , exDelta8
  , exDelta9
  ]

-- | Recursive two-buyers (Example 2 from Less Is More, 2019)
exTwoBuyers :: BenchExample
exTwoBuyers = BenchExample
  { beName = "Two-Buyers"
  , beCitation = Just scalasYoshida2019
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "a"
        , "s ! { query: s ? { price: rec t . b ! { split: b ? { yes: s ! { buy: end }, no: t }, cancel: s ! { no: end } } } }"
        )
      , ( "s"
        , "a ? { query: a ! { price: a ? { buy: end, no: end } } }"
        )
      , ( "b"
        , "rec t . a ? { split: a ! { yes: end, no: t }, cancel: end }"
        )
      ]
  }

-- | MapReduce with 5 participants (from Less Is More, 2019)
exMapReduce5 :: BenchExample
exMapReduce5 = BenchExample
  { beName = "MapReduce-5"
  , beCitation = Just scalasYoshida2019
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "m"
        , "rec t . w1 ! { datum: w2 ! { datum: w3 ! { datum: r ? { continue: t, stop: w1 ! { stop: w2 ! { stop: w3 ! { stop: end } } } } } } }"
        )
      , ( "w1"
        , "m ? { datum: rec t . r ! { result: m ? { datum: t, stop: end } } }"
        )
      , ( "w2"
        , "m ? { datum: rec t . r ! { result: m ? { datum: t, stop: end } } }"
        )
      , ( "w3"
        , "m ? { datum: rec t . r ! { result: m ? { datum: t, stop: end } } }"
        )
      , ( "r"
        , "rec t . w1 ? { result: w2 ? { result: w3 ? { result: m ! { continue: t, stop: end } } } }"
        )
      ]
  }

-- | OAuth2 fragment (Example (1) from Less Is More, 2019)
exOAuth :: BenchExample
exOAuth = BenchExample
  { beName = "OAuth"
  , beCitation = Just scalasYoshida2019
  , beGlobalSource = Just
      "s -> c { login: c -> a { passwd: a -> s { auth: end } }, cancel: c -> a { quit: end } }"
  , beParticipants =
      [ ( "s", "c ! {cancel: end, login: a ? {auth: end}}" )
      , ( "c", "s ? {cancel: a ! {quit: end}, login: a ! {passwd: end}}" )
      , ( "a", "c ? {passwd: s ! {auth: end}, quit: end}" )
      ]
  }

-- | G_ip (Example 4.8 from POPL 2025)
exGIp :: BenchExample
exGIp = BenchExample
  { beName = "G_ip"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Just
      "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l1: t } }"
  , beParticipants =
      [ ( "q", "rec t . r ! { l1: t, l2: t }" )
      , ( "r", "rec t . q ? {l1: p ! {l1: t}, l2: p ! {l1: t}}" )
      , ( "p", "rec t . r ? {l1: t}" )
      ]
  }

-- | G_if (Example 4.8 from POPL 2025)
exGIf :: BenchExample
exGIf = BenchExample
  { beName = "G_if"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Just
      "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l2: end } }"
  , beParticipants =
      [ ( "q", "rec t . r ! {l1: rec t1 . r ! {l1: t1, l2: end}, l2: end}" )
      , ( "r", "rec t . q ? {l1: p ! {l1: t}, l2: p ! {l2: end}}" )
      , ( "p", "rec t . r ? {l1: t, l2: end}" )
      ]
  }

-- | G_itp23 (ITP 2023)
exGItp23 :: BenchExample
exGItp23 = BenchExample
  { beName = "G_itp23"
  , beCitation = Just tiroreEtAl2023
  , beGlobalSource = Just
      "rec t . Alice -> Bob { string: rec t2 . Carl -> Dave { left: t, right: Alice -> Bob { string: t2 } } }"
  , beParticipants =
      [ ( "Alice", "rec t . Bob ! {string: rec t2 . Bob ! {string: t2}}" )
      , ( "Bob", "rec t . Alice ? {string: rec t2 . Alice ? {string: t2}}" )
      , ( "Carl", "rec t . Dave ! {left: t, right: rec t1 . Dave ! {left: t, right: t1}}" )
      , ( "Dave", "rec t . Carl ? {left: t, right: rec t1 . Carl ? {left: t, right: t1}}" )
      ]
  }

-- | G_ring (Ring protocol from POPL 2026)
exGRing :: BenchExample
exGRing = BenchExample
  { beName = "G_ring"
  , beCitation = Just castroPerezEtAl2026
  , beGlobalSource = Just
      ( "a -> b { "
        ++ "AppThenGet: b -> c { AppThenGet: c -> a { Val: end } }, "
        ++ "App: b -> c { App: a -> c { Get: c -> a { Val: end } } } "
        ++ "}"
      )
  , beParticipants =
      [ ( "a", "b ! {App: c ! {Get: c ? {Val: end}}, AppThenGet: c ? {Val: end}}" )
      , ( "b", "a ? {App: c ! {App: end}, AppThenGet: c ! {AppThenGet: end}}" )
      , ( "c", "b ? {App: a ? {Get: a ! {Val: end}}, AppThenGet: a ! {Val: end}}" )
      ]
  }

-- | Delta5 (POPL 2025 Fig 4)
exDelta5 :: BenchExample
exDelta5 = BenchExample
  { beName = "Delta5"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "q", "p ? { l1: r ? { l2: end, l3: end }, l4: r ? { l2: end, l5: end } }" )
      , ( "p", "q ! { l1: end, l4: end }" )
      , ( "r", "q ! { l2: end }" )
      ]
  }

-- | Delta6 (POPL 2025 Fig 4)
exDelta6 :: BenchExample
exDelta6 = BenchExample
  { beName = "Delta6"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "q", "p ? { l1: end, l2: end }" )
      , ( "p", "q ! { l1: end, l3: end }" )
      ]
  }

-- | Delta7 (POPL 2025 Fig 4)
exDelta7 :: BenchExample
exDelta7 = BenchExample
  { beName = "Delta7"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "q", "rec t . p ? { val_S: t }" )
      , ( "p", "rec t . q ! { val_S: t }" )
      , ( "r", "s ? { l2: end }" )
      , ( "s", "r ! { l1: end }" )
      , ( "u", "v ! { l1: end }" )
      ]
  }

-- | Delta8 (POPL 2025 Fig 4)
exDelta8 :: BenchExample
exDelta8 = BenchExample
  { beName = "Delta8"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "q", "p ? { val_S: end }" )
      ]
  }

-- | Delta9 (POPL 2025 Fig 4)
exDelta9 :: BenchExample
exDelta9 = BenchExample
  { beName = "Delta9"
  , beCitation = Just udomYoshida2025
  , beGlobalSource = Nothing
  , beParticipants =
      [ ( "q", "rec t . p ? { val_S: t }" )
      , ( "p", "rec t . q ! { val_S: t }" )
      , ( "r", "s ? { val_bool: end }" )
      ]
  }
