module Benchmark.Generators
  ( mkMapReduce
  , mkMapReduceGlobal
  , mkIndependentWorkers
  , mkIndependentWorkersGlobal
  , mkCcfGlobal
  , mkCcfLocal
  , mkCcfSimple
  , mkBinCounter
  ) where

import Benchmark.Types (Citation(..), GlobalExample(..), LocalExample(..))
import Data.List (intercalate)

-- ---------------------------------------------------------------------------
-- Citations used by generated examples
-- ---------------------------------------------------------------------------

scalasYoshida2019 :: Citation
scalasYoshida2019 = Citation "Scalas2019" "Scalas \\& Yoshida, 2019" Nothing

udomYoshida2025 :: Citation
udomYoshida2025 = Citation "thien-nobuko-popl-25" "Udomsrirungruang \\& Yoshida, 2025" Nothing

-- ---------------------------------------------------------------------------
-- MapReduce (parameterised by number of workers) — local only
-- ---------------------------------------------------------------------------

-- | Generate MapReduce local benchmark with n workers (n+2 participants total).
mkMapReduce :: Int -> LocalExample
mkMapReduce n = LocalExample
  { leName = "G_mr-" ++ show (n + 2)
  , leDisplayName = "MapReduce(" ++ show (n + 2) ++ ")"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~3" }
  , leParticipants = [masterType] ++ workerTypes ++ [reducerType]
  }
  where
    workers = ["w" ++ show i | i <- [1..n]]

    sendDatum w rest = w ++ " ! { datum: " ++ w ++ " ! [int]; " ++ rest ++ " }"
    sendStop  w rest = w ++ " ! { stop: " ++ rest ++ " }"
    recvPayload w rest = w ++ " ? [int]; " ++ rest

    masterSrc = "rec t . " ++ foldr sendDatum recvReducer workers
    recvReducer = "r ? { continue: r ? [int]; t, stop: " ++ foldr sendStop "end" workers ++ " }"
    masterType = ("m", masterSrc)

    workerTypes =
      [(w, "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }") | w <- workers]

    reducerSrc = "rec t . " ++ foldr recvPayload ("m ! { continue: m ! [int]; t, stop: end }") workers
    reducerType = ("r", reducerSrc)

-- | Generate MapReduce global type with n workers and payload sends.
mkMapReduceGlobal :: Int -> GlobalExample
mkMapReduceGlobal n = GlobalExample
  { geName = "G_mr-" ++ show (n + 2)
  , geDisplayName = "MapReduce(" ++ show (n + 2) ++ ")"
  , geCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~3" }
  , geGlobalSource = globalSrc
  , geParticipantNames = ["m"] ++ workers ++ ["r"]
  }
  where
    workers = ["w" ++ show i | i <- [1..n]]

    globalSrc = "rec t . " ++ foldr sendDatum reducer workers

    sendDatum w rest =
      "m -> " ++ w ++ " { datum: m -> " ++ w ++ " [int]; " ++ rest ++ " }"

    reducer =
      concatMap (\w -> w ++ " -> r [int]; ") workers
        ++ "r -> m { "
        ++ "continue: r -> m [int]; t, "
        ++ "stop: " ++ foldr sendStop "end" workers
        ++ " }"

    sendStop w rest =
      "m -> " ++ w ++ " { stop: " ++ rest ++ " }"

-- ---------------------------------------------------------------------------
-- Independent Workers (Example 4 from Less Is More, 2019)
-- ---------------------------------------------------------------------------

-- | Generate Independent Workers global type with n worker triples.
mkIndependentWorkersGlobal :: Int -> GlobalExample
mkIndependentWorkersGlobal n = GlobalExample
  { geName = "G_iw-" ++ show (3 * n + 1)
  , geDisplayName = "Independent Workers(" ++ show (3 * n + 1) ++ ")"
  , geCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~4" }
  , geGlobalSource = globalSrc
  , geParticipantNames = ["s"] ++ concatMap (\i -> [wa i, wb i, wc i]) [1..n]
  }
  where
    wa i = "wa" ++ show i
    wb i = "wb" ++ show i
    wc i = "wc" ++ show i

    globalSrc =
      foldr (\i rest -> "s -> " ++ wa i ++ " { datum: s -> " ++ wa i ++ " [int]; " ++ rest ++ " }")
        (interleaveChains [1..n])
        [1..n]

    -- Interleave independent worker chains so that all participants are
    -- involved in every branch (no closure mixes end with involved).
    -- The structure nests datum/stop choices for all chains first, then
    -- does the work round for chains that chose datum.  When a chain
    -- stops, committed chains finish their pending round before a fresh
    -- recursive sub-protocol begins for the remaining chains.
    interleaveChains [] = "end"
    interleaveChains chains =
      let tag = concatMap show chains
          loopVar = "t" ++ tag
       in "rec " ++ loopVar ++ " . " ++ nestChoices chains [] loopVar

    -- Nest datum/stop choices.
    --   undecided : chains whose wa->wb choice hasn't been emitted yet
    --   committed : chains that already chose datum in this round
    --   loopVar   : rec variable for the current set of chains
    nestChoices [] committed loopVar = doWork committed loopVar
    nestChoices (i:rest) committed loopVar =
      wa i ++ " -> " ++ wb i ++ " { "
        ++ "datum: " ++ nestChoices rest (committed ++ [i]) loopVar ++ ", "
        ++ "stop: " ++ wb i ++ " -> " ++ wc i ++ " { stop: "
          ++ afterStop committed rest ++ " } "
        ++ "}"

    -- After a chain stops: finish the pending round for committed chains,
    -- then start a fresh recursive sub-protocol for all remaining chains.
    afterStop committed rest =
      let remaining = committed ++ rest
       in case remaining of
            [] -> "end"
            _  -> pendingWork committed (interleaveChains remaining)

    -- Emit one round of work (payload + wb->wc forward + payloads back)
    -- for committed chains, threading the continuation inside the braces.
    pendingWork [] cont = cont
    pendingWork (i:rest) cont =
      wa i ++ " -> " ++ wb i ++ " [int]; "
        ++ wb i ++ " -> " ++ wc i ++ " { datum: "
        ++ wb i ++ " -> " ++ wc i ++ " [int]; "
        ++ wc i ++ " -> " ++ wa i ++ " [int]; "
        ++ pendingWork rest cont
        ++ " }"

    -- One round of work for all active chains that chose datum.
    doWork [] loopVar = loopVar
    doWork (i:rest) loopVar =
      wa i ++ " -> " ++ wb i ++ " [int]; "
        ++ wb i ++ " -> " ++ wc i ++ " { datum: "
        ++ wb i ++ " -> " ++ wc i ++ " [int]; "
        ++ wc i ++ " -> " ++ wa i ++ " [int]; "
        ++ doWork rest loopVar
        ++ " }"

-- | Generate Independent Workers local example with n worker triples.
mkIndependentWorkers :: Int -> LocalExample
mkIndependentWorkers n = LocalExample
  { leName = "G_iw-" ++ show (3 * n + 1)
  , leDisplayName = "Independent Workers(" ++ show (3 * n + 1) ++ ")"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~4" }
  , leParticipants = [sType] ++ concatMap workerTriple [1..n]
  }
  where
    wa i = "wa" ++ show i
    wb i = "wb" ++ show i
    wc i = "wc" ++ show i

    sType = ("s", concatMap (\i -> wa i ++ " ! [int]; ") [1..n] ++ "end")

    workerTriple i =
      [ ( wa i
        , "s ? [int]; rec t . " ++ wb i ++ " ! { "
            ++ "datum: " ++ wb i ++ " ! [int]; " ++ wc i ++ " ? [int]; t, "
            ++ "stop: end }"
        )
      , ( wb i
        , "rec t . " ++ wa i ++ " ? { "
            ++ "datum: " ++ wa i ++ " ? [int]; " ++ wc i ++ " ! { "
              ++ "datum: " ++ wc i ++ " ! [int]; t"
            ++ " }, "
            ++ "stop: " ++ wc i ++ " ! { stop: end } }"
        )
      , ( wc i
        , "rec t . " ++ wb i ++ " ? { "
            ++ "datum: " ++ wb i ++ " ? [int]; " ++ wa i ++ " ! [int]; t, "
            ++ "stop: end }"
        )
      ]

-- ---------------------------------------------------------------------------
-- G_cf: coinductive-full context (POPL 2025, Section 4.4, Theorem 4.24)
-- ---------------------------------------------------------------------------

-- | Shared parts for G_cf generation.
data CcfParts = CcfParts
  { ccfGlobalType :: String
  , ccfPType      :: String
  , ccfRType      :: String
  , ccfLabels     :: [String]
  , ccfPrimes     :: [Int]
  }

mkCcfParts :: Int -> CcfParts
mkCcfParts size = CcfParts
  { ccfGlobalType = globalType
  , ccfPType      = pType
  , ccfRType      = rType
  , ccfLabels     = labels
  , ccfPrimes     = ns
  }
  where
    ns = take size consecutivePrimes
    labels = ["l" ++ show i | i <- [1..size]]

    globalType = "p -> r { " ++ intercalate ", " gBranches ++ " }"
    gBranches = zipWith gBranch labels ns
    gBranch l n =
      l ++ " : rec t . p -> q { a : " ++ nestGlobal (n - 1)
        ++ ", b : p -> q { " ++ l ++ " : end } }"
    nestGlobal 0 = "t"
    nestGlobal k = "p -> q { a : " ++ nestGlobal (k - 1) ++ " }"

    pType = "r ! { " ++ intercalate ", " pBranches ++ " }"
    pBranches = zipWith pBranch labels ns
    pBranch l n =
      l ++ " : rec t . q ! { a : " ++ nestSend (n - 1)
        ++ ", b : q ! { " ++ l ++ " : end } }"
    nestSend 0 = "t"
    nestSend k = "q ! { a : " ++ nestSend (k - 1) ++ " }"

    rType = "p ? { " ++ intercalate ", " [l ++ " : end" | l <- labels] ++ " }"

-- | Full q type for G_cf (coinductive full merge, cycle length lcm).
fullQType :: [String] -> [Int] -> String
fullQType labels ns =
  let cycleLen = foldl1 lcm ns
      qState i
        | i == cycleLen - 1 = withB i "t"
        | otherwise         = withB i (qState (i + 1))
      withB i inner =
        let bLabels = [labels !! j | (j, n) <- zip [0..] ns, i `mod` n == 0]
         in if null bLabels
            then "p ? { a : " ++ inner ++ " }"
            else "p ? { a : " ++ inner ++ ", b : p ? { "
                  ++ intercalate ", " [l ++ " : end" | l <- bLabels] ++ " } }"
   in "rec t . " ++ qState 0

-- | Simplified q type for G_cfs (constant-size).
simpleQType :: [String] -> String
simpleQType labels =
  "rec t . p ? { a : t, b : p ? { "
    ++ intercalate ", " [l ++ " : end" | l <- labels]
    ++ " } }"

-- | G_cf global example.
mkCcfGlobal :: Int -> GlobalExample
mkCcfGlobal size =
  let parts = mkCcfParts size
  in GlobalExample
    { geName = "G_cf-" ++ show size
    , geDisplayName = "Coinductive Full(" ++ show size ++ ")"
    , geCitation = Just udomYoshida2025 { citeRef = Just "Thm.~4.24" }
    , geGlobalSource = ccfGlobalType parts
    , geParticipantNames = ["p", "q", "r"]
    }

-- | G_cf local example (with full lcm-based q type).
mkCcfLocal :: Int -> LocalExample
mkCcfLocal size =
  let parts = mkCcfParts size
      qType = fullQType (ccfLabels parts) (ccfPrimes parts)
  in LocalExample
    { leName = "G_cf-" ++ show size
    , leDisplayName = "Coinductive Full(" ++ show size ++ ")"
    , leCitation = Just udomYoshida2025 { citeRef = Just "Thm.~4.24" }
    , leParticipants = [("p", ccfPType parts), ("q", qType), ("r", ccfRType parts)]
    }

-- | G_cfs local example (simplified q, constant-size).
mkCcfSimple :: Int -> LocalExample
mkCcfSimple size =
  let parts = mkCcfParts size
      qType = simpleQType (ccfLabels parts)
  in LocalExample
    { leName = "G_cfs-" ++ show size
    , leDisplayName = "Coinductive Full Optimised(" ++ show size ++ ")"
    , leCitation = Just udomYoshida2025 { citeRef = Just "Thm.~4.24" }
    , leParticipants = [("p", ccfPType parts), ("q", qType), ("r", ccfRType parts)]
    }

-- | First few primes (sufficient for benchmark generation).
consecutivePrimes :: [Int]
consecutivePrimes = [2, 3, 5, 7, 11, 13]

-- ---------------------------------------------------------------------------
-- BinCounter: n-bit binary counter — local only
-- ---------------------------------------------------------------------------

-- | Generate an n-bit binary counter benchmark.
mkBinCounter :: Int -> LocalExample
mkBinCounter n = LocalExample
  { leName = "BinCtr-" ++ show n
  , leDisplayName = "Binary Counter(" ++ show n ++ ")"
  , leCitation = Nothing
  , leParticipants = [srcType] ++ bitTypes ++ [ovfType]
  }
  where
    bitName :: Int -> String
    bitName i = "p" ++ show i
    lastBit   = n - 1

    srcType = ("src", "rec t . " ++ bitName 0 ++ " ! { l0 : t, l1 : t }")
    ovfType = ("ovf", "rec t . " ++ bitName lastBit ++ " ? { l0 : t, l1 : t }")

    bitTypes = [bitType i | i <- [0 .. lastBit]]

    bitType i =
      let left  = if i == 0       then "src" else bitName (i - 1)
          right = if i == lastBit then "ovf" else bitName (i + 1)
       in (bitName i, bitLocalType left right)

    bitLocalType left right =
      "rec t0 . " ++ left ++ " ? { "
        ++ "l0 : " ++ right ++ " ! { l0 : t0 }, "
        ++ "l1 : " ++ right ++ " ! { l0 : "
          ++ "rec t1 . " ++ left ++ " ? { "
            ++ "l0 : " ++ right ++ " ! { l0 : t1 }, "
            ++ "l1 : " ++ right ++ " ! { l1 : t0 }"
          ++ " }"
        ++ " }"
      ++ " }"
