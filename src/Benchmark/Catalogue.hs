module Benchmark.Catalogue
  ( Citation(..)
  , GlobalExample(..)
  , LocalExample(..)
  , ParsedGlobalExample(..)
  , ParsedLocalExample(..)
  , globalExamples
  , localExamples
  , parseGlobalExample
  , parseLocalExample
  ) where

import Benchmark.Generators
  ( mkBinCounter
  , mkCcfGlobal
  , mkCcfLocal
  , mkCcfSimple
  , mkIndependentWorkers
  , mkIndependentWorkersGlobal
  , mkMapReduce
  , mkMapReduceGlobal
  )
import Benchmark.QBF (mkQBF, qbfGame)
import Benchmark.Types

-- ---------------------------------------------------------------------------
-- Citations
-- ---------------------------------------------------------------------------

scalasYoshida2019 :: Citation
scalasYoshida2019 = Citation "Scalas2019" "Scalas \\& Yoshida, 2019" Nothing

udomYoshida2025 :: Citation
udomYoshida2025 = Citation "thien-nobuko-popl-25" "Udomsrirungruang \\& Yoshida, 2025" Nothing

tiroreEtAl2023 :: Citation
tiroreEtAl2023 = Citation "Tirore2023" "Tirore et al., 2023" Nothing

castroPerezEtAl2026 :: Citation
castroPerezEtAl2026 = Citation "Castro2026" "Castro-Perez et al., 2026" Nothing

liEtAl2023 :: Citation
liEtAl2023 = Citation "Li2023" "Li et al., 2023" Nothing

yoshidaGheri2020 :: Citation
yoshidaGheri2020 = Citation "YoshidaGheri2020" "Yoshida \\& Gheri, 2020" Nothing

gheriYoshida2022 :: Citation
gheriYoshida2022 = Citation "DBLP:conf/ecoop/GheriLSTY22" "Gheri \\& Yoshida, 2022" Nothing

neykovaEtAl2013 :: Citation
neykovaEtAl2013 = Citation "DBLP:conf/rv/NeykovaYH13" "Neykova et al., 2013" Nothing

huYoshida2016 :: Citation
huYoshida2016 = Citation "FASE16EndpointAPI" "Hu \\& Yoshida, 2016" Nothing

lagaillardieEtAl2022 :: Citation
lagaillardieEtAl2022 = Citation "DBLP:conf/ecoop/LagaillardieNY22" "Lagaillardie et al., 2022" Nothing


-- ---------------------------------------------------------------------------
-- Global examples (have a global type for projection benchmarks)
-- ---------------------------------------------------------------------------

globalExamples :: [GlobalExample]
globalExamples =
  [ gSimpleTravelAgency
  , gBetterTravelAgency
  , gOAuth
  , gTwoBuyer
  , gGIp
  , gGIf
  , gGItp23
  , gGRing
  , gGOddEven
  , gMonteCarloMap
  , gMonteCarloMin
  , gIndependentPairs
  , gCompany
  , gOnlineWallet
  , gAdder
  , gDistLog
  , gEVoting
  , mkMapReduceGlobal 3
  , mkMapReduceGlobal 4
  , mkMapReduceGlobal 5
  , mkIndependentWorkersGlobal 2
  , mkIndependentWorkersGlobal 3
  , mkCcfGlobal 2
  , mkCcfGlobal 3
  , mkCcfGlobal 4
  , mkCcfGlobal 5
  ]

-- | G_sta Simple Travel Agency (Fig 1(a) from A Very Gentle Introduction to MPST)
gSimpleTravelAgency :: GlobalExample
gSimpleTravelAgency = GlobalExample
  { geName = "G_sta"
  , geDisplayName = "Simple Travel Agency"
  , geCitation = Just yoshidaGheri2020 { citeRef = Just "Fig.~1(a)" }
  , geGlobalSource =
      "c -> a [string]; a -> c [int]; c -> a { "
        ++ "accept: c -> a [string]; a -> c [int]; end, "
        ++ "reject: end "
        ++ "}"
  , geParticipantNames = ["c", "a"]
  }

-- | G_bta Better Travel Agency (Fig 1(b) from A Very Gentle Introduction to MPST)
gBetterTravelAgency :: GlobalExample
gBetterTravelAgency = GlobalExample
  { geName = "G_bta"
  , geDisplayName = "Better Travel Agency"
  , geCitation = Just yoshidaGheri2020 { citeRef = Just "Fig.~1(b)" }
  , geGlobalSource =
      "rec t . c -> a [string]; a -> c [int]; c -> a { "
        ++ "accept: c -> a [string]; a -> c [int]; end, "
        ++ "retry: t, "
        ++ "reject: end "
        ++ "}"
  , geParticipantNames = ["c", "a"]
  }

-- | G_oa OAuth (Example 1 from Less Is More, 2019)
gOAuth :: GlobalExample
gOAuth = GlobalExample
  { geName = "G_oa"
  , geDisplayName = "OAuth"
  , geCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~1" }
  , geGlobalSource =
      "s -> c { "
        ++ "login: c -> a { password: c -> a [string]; a -> s { auth: a -> s [bool]; end } }, "
        ++ "auth: c -> a { quit: end } "
        ++ "}"
  , geParticipantNames = ["s", "c", "a"]
  }

-- | G_tb Recursive Two-Buyer (Example 2 from Less Is More, 2019)
gTwoBuyer :: GlobalExample
gTwoBuyer = GlobalExample
  { geName = "G_tb"
  , geDisplayName = "Two Buyer"
  , geCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~2" }
  , geGlobalSource =
      "a -> s { query: a -> s [string]; s -> a { price: s -> a [int]; "
        ++ "rec t . a -> b { "
        ++ "split: b -> a { yes: a -> s { buy: end }, no: t }, "
        ++ "cancel: a -> s { no: end } "
        ++ "} } }"
  , geParticipantNames = ["a", "s", "b"]
  }

-- | G_ip (Example 4.8 from POPL 2025)
gGIp :: GlobalExample
gGIp = GlobalExample
  { geName = "G_ip"
  , geDisplayName = "Inductive Plain"
  , geCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , geGlobalSource =
      "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l1: t } }"
  , geParticipantNames = ["q", "r", "p"]
  }

-- | G_if (Example 4.8 from POPL 2025)
gGIf :: GlobalExample
gGIf = GlobalExample
  { geName = "G_if"
  , geDisplayName = "Inductive Full"
  , geCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , geGlobalSource =
      "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l2: end } }"
  , geParticipantNames = ["q", "r", "p"]
  }

-- | G_itp (ITP 2023)
gGItp23 :: GlobalExample
gGItp23 = GlobalExample
  { geName = "G_itp"
  , geDisplayName = "Semantic Recursion"
  , geCitation = Just tiroreEtAl2023
  , geGlobalSource =
      "rec t . a -> b [string]; rec t2 . c -> d { left: t, right: a -> b [string]; t2 }"
  , geParticipantNames = ["a", "b", "c", "d"]
  }

-- | G_ring (Ring protocol from POPL 2026)
gGRing :: GlobalExample
gGRing = GlobalExample
  { geName = "G_ring"
  , geDisplayName = "Ring"
  , geCitation = Just castroPerezEtAl2026
  , geGlobalSource =
      "a -> b { "
        ++ "AppThenGet: b -> c { AppThenGet: c -> a { Val: end } }, "
        ++ "App: b -> c { App: a -> c { Get: c -> a { Val: end } } } "
        ++ "}"
  , geParticipantNames = ["a", "b", "c"]
  }

-- | G_oe "odd-even" (Example 2.1 from Li et al., CAV 2023)
gGOddEven :: GlobalExample
gGOddEven = GlobalExample
  { geName = "G_oe"
  , geDisplayName = "Odd-Even"
  , geCitation = Just liEtAl2023 { citeRef = Just "Ex.~2.1" }
  , geGlobalSource =
      "p -> q { "
        ++ "o : q -> r { o : rec t1 . p -> q { "
          ++ "o : q -> r { o : q -> r { o : t1 } }, "
          ++ "b : q -> r { b : r -> p { o : end } } "
        ++ "} }, "
        ++ "m : rec t2 . p -> q { "
          ++ "o : q -> r { o : q -> r { o : t2 } }, "
          ++ "b : q -> r { b : r -> p { m : end } } "
        ++ "} "
        ++ "}"
  , geParticipantNames = ["p", "q", "r"]
  }

-- | G_map (Monte Carlo running example from this paper)
gMonteCarloMap :: GlobalExample
gMonteCarloMap = GlobalExample
  { geName = "G_mc_map"
  , geDisplayName = "Monte Carlo Gmap"
  , geCitation = Nothing
  , geGlobalSource =
      "rec t . "
        ++ "m -> w1 { map: m -> w1 [float]; w1 -> r [float]; "
        ++ "m -> w2 { map: m -> w2 [float]; w2 -> r [float]; "
        ++ "r -> m { cont: t, stop: m -> w1 { stop: m -> w2 { stop: end } } } "
        ++ "} }"
  , geParticipantNames = ["m", "w1", "w2", "r"]
  }

-- | G_min (strictly smaller Monte Carlo protocol from this paper)
gMonteCarloMin :: GlobalExample
gMonteCarloMin = GlobalExample
  { geName = "G_mc_min"
  , geDisplayName = "Monte Carlo Gmin"
  , geCitation = Nothing
  , geGlobalSource =
      "m -> w1 { map: m -> w1 [float]; "
        ++ "m -> w2 { map: m -> w2 [float]; "
        ++ "w1 -> r [float]; w2 -> r [float]; "
        ++ "r -> m { stop: m -> w1 { stop: m -> w2 { stop: end } } } "
        ++ "} }"
  , geParticipantNames = ["m", "w1", "w2", "r"]
  }

-- | Two independent communicating pairs (used in synthesis tests)
gIndependentPairs :: GlobalExample
gIndependentPairs = GlobalExample
  { geName = "G_pairs"
  , geDisplayName = "Independent Pairs"
  , geCitation = Nothing
  , geGlobalSource =
      "a -> b { "
        ++ "l1: c -> d { l1: end, l2: end }, "
        ++ "l2: c -> d { l1: end, l2: end } "
        ++ "}"
  , geParticipantNames = ["a", "b", "c", "d"]
  }


-- | G_company Company Communication (Fig. 9 from Gheri & Yoshida, 2022)
gCompany :: GlobalExample
gCompany = GlobalExample
  { geName = "G_company"
  , geDisplayName = "Company Communication"
  , geCitation = Just gheriYoshida2022 { citeRef = Just "Fig.~9" }
  , geGlobalSource =
      "d -> ad { "
        ++ "prod: d -> s { "
          ++ "prod: d -> f1 { "
            ++ "prod: f1 -> f2 { "
              ++ "prod: rec t . f2 -> f1 { "
                ++ "price: f1 -> d { "
                  ++ "ok: d -> ad { "
                    ++ "go: f1 -> s { "
                      ++ "price: s -> w { "
                        ++ "publish: end "
                      ++ "} "
                    ++ "} "
                  ++ "} "
                ++ "}, "
                ++ "wait: f1 -> d { "
                  ++ "wait: d -> ad { "
                    ++ "wait: f1 -> s { "
                      ++ "wait: s -> w { "
                        ++ "wait: t "
                      ++ "} "
                    ++ "} "
                  ++ "} "
                ++ "} "
              ++ "} "
            ++ "} "
          ++ "} "
        ++ "} "
      ++ "}"
  , geParticipantNames = ["d", "ad", "s", "f1", "f2", "w"]
  }

-- | G_ow Online Wallet (SPY, Neykova et al., RV 2013)
gOnlineWallet :: GlobalExample
gOnlineWallet = GlobalExample
  { geName = "G_ow"
  , geDisplayName = "Online Wallet"
  , geCitation = Just neykovaEtAl2013 { citeRef = Just "Fig.~1" }
  , geGlobalSource =
      "c -> a { login: c -> a [string]; c -> a [string]; "
        ++ "a -> c { "
          ++ "login_ok: a -> s { "
            ++ "login_ok: rec t . s -> c { "
              ++ "account: s -> c [int]; s -> c [int]; "
                ++ "c -> s { "
                  ++ "pay: c -> s [string]; c -> s [int]; t, "
                  ++ "quit: end "
                ++ "} "
              ++ "} "
            ++ "}, "
          ++ "login_fail: a -> c [string]; "
            ++ "a -> s { login_fail: a -> s [string]; end } "
        ++ "} }"
  , geParticipantNames = ["c", "a", "s"]
  }

-- | G_adder Adder (Fig. 1(a) from Hu & Yoshida, FASE 2016)
gAdder :: GlobalExample
gAdder = GlobalExample
  { geName = "G_adder"
  , geDisplayName = "Adder"
  , geCitation = Just huYoshida2016 { citeRef = Just "Fig.~1(a)" }
  , geGlobalSource =
      "rec t . c -> s { "
        ++ "Add: c -> s [int]; c -> s [int]; s -> c { Res: s -> c [int]; t }, "
        ++ "Bye: s -> c { Bye: end } "
        ++ "}"
  , geParticipantNames = ["c", "s"]
  }

-- | G_dl Distributed Logging (Fig. 16 from Lagaillardie et al., 2022)
gDistLog :: GlobalExample
gDistLog = GlobalExample
  { geName = "G_dl"
  , geDisplayName = "Distributed Logging"
  , geCitation = Just lagaillardieEtAl2022 { citeRef = Just "Fig.~16" }
  , geGlobalSource =
      "c -> s { Start: c -> s [int]; rec t . s -> c { "
        ++ "Success: s -> c [int]; t, "
        ++ "Failure: s -> c [int]; c -> s { "
          ++ "Restart: c -> s [int]; t, "
          ++ "Stop: c -> s [int]; end "
        ++ "} } }"
  , geParticipantNames = ["c", "s"]
  }

-- | G_ev E-Voting (Lagaillardie et al., 2022)
gEVoting :: GlobalExample
gEVoting = GlobalExample
  { geName = "G_ev"
  , geDisplayName = "E-Voting"
  , geCitation = Just lagaillardieEtAl2022
  , geGlobalSource =
      "v -> s { Authenticate: v -> s [string]; s -> v { "
        ++ "Ok: s -> v [string]; v -> s { "
          ++ "Yes: v -> s [string]; s -> v { Result: s -> v [int]; end }, "
          ++ "No: v -> s [string]; s -> v { Result: s -> v [int]; end } "
        ++ "}, "
        ++ "Reject: s -> v [string]; end "
        ++ "} }"
  , geParticipantNames = ["v", "s"]
  }


-- ---------------------------------------------------------------------------
-- Local examples (have local type context for synthesis benchmarks)
-- ---------------------------------------------------------------------------

localExamples :: [LocalExample]
localExamples =
  -- Classic examples
  [ lTwoBuyers
  , lMapReduce5
  , mkMapReduce 4
  , mkMapReduce 5
  , lOAuth
  , mkIndependentWorkers 2
  , mkIndependentWorkers 3
  , lGIp
  , lGIf
  , lGItp23
  , lGRing
  , lGOddEven
  , lMonteCarloMap
  , lMonteCarloMin
  , lIndependentPairs
  , lOnlineWallet
  , lAdder
  , lDistLog
  , lEVoting
  -- Delta examples (local only)
  , lDelta5
  , lDelta6
  , lDelta7
  , lDelta8
  , lDelta9
  -- G_cf family
  , mkCcfLocal 2
  , mkCcfLocal 3
  , mkCcfLocal 4
  , mkCcfLocal 5
  -- G_cfs family
  , mkCcfSimple 2
  , mkCcfSimple 3
  , mkCcfSimple 4
  , mkCcfSimple 5
  -- Binary counter family
  , mkBinCounter 2
  , mkBinCounter 3
  , mkBinCounter 4
  , mkBinCounter 5
  -- QBF encoding (removed from submission)
  -- , mkQBF qbfGame
  ]

-- | G_tb Recursive Two-Buyer (Example 2 from Less Is More, 2019)
lTwoBuyers :: LocalExample
lTwoBuyers = LocalExample
  { leName = "G_tb"
  , leDisplayName = "Two Buyer"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~2" }
  , leParticipants =
      [ ( "a"
        , "s ! { query: s ! [string]; s ? { price: s ? [int]; rec t . b ! { split: b ? { yes: s ! { buy: end }, no: t }, cancel: s ! { no: end } } } }"
        )
      , ( "s"
        , "a ? { query: a ? [string]; a ! { price: a ! [int]; a ? { buy: end, no: end } } }"
        )
      , ( "b"
        , "rec t . a ? { cancel: end, split: a ! { no: t, yes: end } }"
        )
      ]
  }

-- | MapReduce with 5 participants (Example 3 from Less Is More, 2019)
lMapReduce5 :: LocalExample
lMapReduce5 = LocalExample
  { leName = "G_mr-5"
  , leDisplayName = "MapReduce(5)"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~3" }
  , leParticipants =
      [ ( "m"
        , "rec t . w1 ! { datum: w1 ! [int]; w2 ! { datum: w2 ! [int]; w3 ! { datum: w3 ! [int]; "
          ++ "r ? { continue: r ? [int]; t, stop: w1 ! { stop: w2 ! { stop: w3 ! { stop: end } } } } } } }"
        )
      , ( "w1"
        , "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }"
        )
      , ( "w2"
        , "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }"
        )
      , ( "w3"
        , "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }"
        )
      , ( "r"
        , "rec t . w1 ? [int]; w2 ? [int]; w3 ? [int]; m ! { continue: m ! [int]; t, stop: end }"
        )
      ]
  }

-- | G_oa OAuth (Example 1 from Less Is More, 2019)
lOAuth :: LocalExample
lOAuth = LocalExample
  { leName = "G_oa"
  , leDisplayName = "OAuth"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~1" }
  , leParticipants =
      [ ( "s", "c ! {auth: end, login: a ? {auth: a ? [bool]; end}}" )
      , ( "c", "s ? {auth: a ! {quit: end}, login: a ! {password: a ! [string]; end}}" )
      , ( "a", "c ? {password: c ? [string]; s ! {auth: s ! [bool]; end}, quit: end}" )
      ]
  }

-- | G_ip (Example 4.8 from POPL 2025)
lGIp :: LocalExample
lGIp = LocalExample
  { leName = "G_ip"
  , leDisplayName = "Inductive Plain"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , leParticipants =
      [ ( "q", "rec t . r ! { l1: t, l2: t }" )
      , ( "r", "rec t . q ? {l1: p ! {l1: t}, l2: p ! {l1: t}}" )
      , ( "p", "rec t . r ? {l1: t}" )
      ]
  }

-- | G_if (Example 4.8 from POPL 2025)
lGIf :: LocalExample
lGIf = LocalExample
  { leName = "G_if"
  , leDisplayName = "Inductive Full"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , leParticipants =
      [ ( "q", "rec t . r ! {l1: rec t1 . r ! {l1: t1, l2: end}, l2: end}" )
      , ( "r", "rec t . q ? {l1: p ! {l1: t}, l2: p ! {l2: end}}" )
      , ( "p", "rec t . r ? {l1: t, l2: end}" )
      ]
  }

-- | G_itp (ITP 2023)
lGItp23 :: LocalExample
lGItp23 = LocalExample
  { leName = "G_itp"
  , leDisplayName = "Semantic Recursion"
  , leCitation = Just tiroreEtAl2023
  , leParticipants =
      [ ( "a", "rec t . b ! [string]; t" )
      , ( "b", "rec t . a ? [string]; t" )
      , ( "c", "rec t . rec t2 . d ! { left: t, right: t2 }" )
      , ( "d", "rec t . rec t2 . c ? { left: t, right: t2 }" )
      ]
  }

-- | G_ring (Ring protocol from POPL 2026)
lGRing :: LocalExample
lGRing = LocalExample
  { leName = "G_ring"
  , leDisplayName = "Ring"
  , leCitation = Just castroPerezEtAl2026
  , leParticipants =
      [ ( "a", "b ! {App: c ! {Get: c ? {Val: end}}, AppThenGet: c ? {Val: end}}" )
      , ( "b", "a ? {App: c ! {App: end}, AppThenGet: c ! {AppThenGet: end}}" )
      , ( "c", "b ? {App: a ? {Get: a ! {Val: end}}, AppThenGet: a ! {Val: end}}" )
      ]
  }

-- | G_oe "odd-even" (Example 2.1 from Li et al., CAV 2023)
lGOddEven :: LocalExample
lGOddEven = LocalExample
  { leName = "G_oe"
  , leDisplayName = "Odd-Even"
  , leCitation = Just liEtAl2023 { citeRef = Just "Ex.~2.1" }
  , leParticipants =
      [ ( "p"
        , "q ! { o : rec t1 . q ! { o : t1, b : r ? { o : end } }, "
          ++ "m : rec t2 . q ! { o : t2, b : r ? { m : end } } }"
        )
      , ( "q"
        , "p ? { o : r ! { o : rec t1 . p ? { o : r ! { o : r ! { o : t1 } }, b : r ! { b : end } } }, "
          ++ "m : rec t2 . p ? { o : r ! { o : r ! { o : t2 } }, b : r ! { b : end } } }"
        )
      , ( "r"
        , "q ? { b: p ! { m: end }, o: rec t . q ? { b: p ! { o: end }, o: q ? { b: p ! { m: end }, o: t } } }"
        )
      ]
  }

-- | G_map via the inferred Monte Carlo local context from this paper.
lMonteCarloMap :: LocalExample
lMonteCarloMap = LocalExample
  { leName = "G_mc_map"
  , leDisplayName = "Monte Carlo Gmap"
  , leCitation = Nothing
  , leParticipants =
      [ ( "m"
        , "rec t . "
          ++ "w1 ! { map: w1 ! [float]; "
          ++ "w2 ! { map: w2 ! [float]; "
          ++ "r ? { cont: t, crash: end, stop: w1 ! { stop: w2 ! { stop: end } } } "
          ++ "} }"
        )
      , ( "w1"
        , "rec t . m ? { map: m ? [float]; r ! [float]; t, stop: end }"
        )
      , ( "w2"
        , "rec t . m ? { map: m ? [float]; r ! [float]; t, stop: end }"
        )
      , ( "r"
        , "rec t . w1 ? [float]; w2 ? [float]; m ! { cont: t, stop: end }"
        )
      ]
  }

-- | G_min via the Monte Carlo variant whose reducer stops after one round.
lMonteCarloMin :: LocalExample
lMonteCarloMin = LocalExample
  { leName = "G_mc_min"
  , leDisplayName = "Monte Carlo Gmin"
  , leCitation = Nothing
  , leParticipants =
      [ ( "m"
        , "rec t . "
          ++ "w1 ! { map: w1 ! [float]; "
          ++ "w2 ! { map: w2 ! [float]; "
          ++ "r ? { cont: t, crash: end, stop: w1 ! { stop: w2 ! { stop: end } } } "
          ++ "} }"
        )
      , ( "w1"
        , "rec t . m ? { map: m ? [float]; r ! [float]; t, stop: end }"
        )
      , ( "w2"
        , "rec t . m ? { map: m ? [float]; r ! [float]; t, stop: end }"
        )
      , ( "r"
        , "w1 ? [float]; w2 ? [float]; m ! { stop: end }"
        )
      ]
  }

-- | Two independent communicating pairs (used in synthesis tests).
lIndependentPairs :: LocalExample
lIndependentPairs = LocalExample
  { leName = "G_pairs"
  , leDisplayName = "Independent Pairs"
  , leCitation = Nothing
  , leParticipants =
      [ ("a", "b ! { l1: end, l2: end }")
      , ("b", "a ? { l1: end, l2: end, l3: end }")
      , ("c", "d ! { l1: end, l2: end }")
      , ("d", "c ? { l1: end, l2: end, l3: end }")
      ]
  }


-- | G_ow Online Wallet (SPY, Neykova et al., RV 2013)
lOnlineWallet :: LocalExample
lOnlineWallet = LocalExample
  { leName = "G_ow"
  , leDisplayName = "Online Wallet"
  , leCitation = Just neykovaEtAl2013 { citeRef = Just "Fig.~1" }
  , leParticipants =
      [ ( "c"
        , "a ! {login: a ! [string]; a ! [string]; a ? {login_fail: a ? [string]; end, "
          ++ "login_ok: rec t . s ? {account: s ? [int]; s ? [int]; s ! {pay: s ! [string]; s ! [int]; t, quit: end}}}}"
        )
      , ( "a"
        , "c ? {login: c ? [string]; c ? [string]; c ! {login_fail: c ! [string]; s ! {login_fail: s ! [string]; end}, "
          ++ "login_ok: s ! {login_ok: end}}}"
        )
      , ( "s"
        , "a ? {login_fail: a ? [string]; end, "
          ++ "login_ok: rec t . c ! {account: c ! [int]; c ! [int]; c ? {pay: c ? [string]; c ? [int]; t, quit: end}}}"
        )
      ]
  }

-- | G_adder Adder (Fig. 1(a) from Hu & Yoshida, FASE 2016)
lAdder :: LocalExample
lAdder = LocalExample
  { leName = "G_adder"
  , leDisplayName = "Adder"
  , leCitation = Just huYoshida2016 { citeRef = Just "Fig.~1(a)" }
  , leParticipants =
      [ ( "c"
        , "rec t . s ! {Add: s ! [int]; s ! [int]; s ? {Res: s ? [int]; t}, Bye: s ? {Bye: end}}"
        )
      , ( "s"
        , "rec t . c ? {Add: c ? [int]; c ? [int]; c ! {Res: c ! [int]; t}, Bye: c ! {Bye: end}}"
        )
      ]
  }

-- | G_dl Distributed Logging (Fig. 16 from Lagaillardie et al., 2022)
lDistLog :: LocalExample
lDistLog = LocalExample
  { leName = "G_dl"
  , leDisplayName = "Distributed Logging"
  , leCitation = Just lagaillardieEtAl2022 { citeRef = Just "Fig.~16" }
  , leParticipants =
      [ ( "c"
        , "s ! {Start: s ! [int]; rec t . s ? {Failure: s ? [int]; s ! {Restart: s ! [int]; t, Stop: s ! [int]; end}, "
          ++ "Success: s ? [int]; t}}"
        )
      , ( "s"
        , "c ? {Start: c ? [int]; rec t . c ! {Failure: c ! [int]; c ? {Restart: c ? [int]; t, Stop: c ? [int]; end}, "
          ++ "Success: c ! [int]; t}}"
        )
      ]
  }

-- | G_ev E-Voting (Lagaillardie et al., 2022)
lEVoting :: LocalExample
lEVoting = LocalExample
  { leName = "G_ev"
  , leDisplayName = "E-Voting"
  , leCitation = Just lagaillardieEtAl2022
  , leParticipants =
      [ ( "v"
        , "s ! {Authenticate: s ! [string]; s ? {Ok: s ? [string]; s ! {No: s ! [string]; s ? {Result: s ? [int]; end}, "
          ++ "Yes: s ! [string]; s ? {Result: s ? [int]; end}}, Reject: s ? [string]; end}}"
        )
      , ( "s"
        , "v ? {Authenticate: v ? [string]; v ! {Ok: v ! [string]; v ? {No: v ? [string]; v ! {Result: v ! [int]; end}, "
          ++ "Yes: v ? [string]; v ! {Result: v ! [int]; end}}, Reject: v ! [string]; end}}"
        )
      ]
  }

-- | Delta5 (POPL 2025 Fig 4)
lDelta5 :: LocalExample
lDelta5 = LocalExample
  { leName = "Delta5"
  , leDisplayName = "$\\ctx[5]$"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "p ? { l1: r ? { l2: end, l3: end }, l4: r ? { l2: end, l5: end } }" )
      , ( "p", "q ! { l1: end, l4: end }" )
      , ( "r", "q ! { l2: end }" )
      ]
  }

-- | Delta6 (POPL 2025 Fig 4)
lDelta6 :: LocalExample
lDelta6 = LocalExample
  { leName = "Delta6"
  , leDisplayName = "$\\ctx[6]$"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "p ? { l1: end, l2: end }" )
      , ( "p", "q ! { l1: end, l3: end }" )
      ]
  }

-- | Delta7 (POPL 2025 Fig 4)
lDelta7 :: LocalExample
lDelta7 = LocalExample
  { leName = "Delta7"
  , leDisplayName = "$\\ctx[7]$"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "rec t . p ? { val_S: t }" )
      , ( "p", "rec t . q ! { val_S: t }" )
      , ( "r", "s ? { l2: end }" )
      , ( "s", "r ! { l1: end }" )
      , ( "u", "v ! { l1: end }" )
      ]
  }

-- | Delta8 (POPL 2025 Fig 4)
lDelta8 :: LocalExample
lDelta8 = LocalExample
  { leName = "Delta8"
  , leDisplayName = "$\\ctx[8]$"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "p ? { val_S: end }" )
      ]
  }

-- | Delta9 (POPL 2025 Fig 4)
lDelta9 :: LocalExample
lDelta9 = LocalExample
  { leName = "Delta9"
  , leDisplayName = "$\\ctx[9]$"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "rec t . p ? { val_S: t }" )
      , ( "p", "rec t . q ! { val_S: t }" )
      , ( "r", "s ? { val_bool: end }" )
      ]
  }
