module Benchmark.QBF
  ( -- * QBF AST
    Var(..)
  , Lit(..)
  , Clause
  , Quantifier(..)
  , QBF(..)
    -- * Encoder
  , mkQBF
  , mkQBFWithBad
    -- * Example formulas
  , qbfTrue1
  , qbfFalse1
  , qbfTrue2
  , qbfFalse2
  , qbfGame
  ) where

import Benchmark.Types (Citation(..), LocalExample(..))
import Data.List (intercalate)

udomYoshida2025 :: Citation
udomYoshida2025 = Citation "thien-nobuko-popl-25" "Udomsrirungruang \\& Yoshida, 2025" Nothing

-- | Variable (1-based index).
newtype Var = Var Int deriving (Eq, Ord, Show)

-- | Literal: positive or negative.
data Lit = Pos Var | Neg Var deriving (Eq, Show)

-- | 3-CNF clause (exactly three literals).
type Clause = (Lit, Lit, Lit)

-- | Quantifier.
data Quantifier = Exists | ForAll deriving (Eq, Show)

-- | QBF in prenex 3-CNF: Q1 v1 ... Qn vn. C1 /\ ... /\ Cm
data QBF = QBF [(Quantifier, Var)] [Clause] deriving (Show)

-- ---------------------------------------------------------------------------
-- Participant naming
-- ---------------------------------------------------------------------------

pName :: Int -> String
pName i = "p" ++ show i

rName :: Int -> String
rName i = "r" ++ show i

-- p[0] = s
leftOfP :: Int -> String
leftOfP 1 = "s"
leftOfP i = pName (i - 1)

-- p[n+1] = r[1]
rightOfP :: Int -> Int -> String
rightOfP n i
  | i == n    = rName 1
  | otherwise = pName (i + 1)

-- r[0] = p[n]
leftOfR :: Int -> Int -> String
leftOfR n 1 = pName n
leftOfR _ i = rName (i - 1)

rightOfR :: Int -> String
rightOfR i = rName (i + 1)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

resolve :: Quantifier -> String
resolve Exists = "tt"
resolve ForAll = "ff"

resolveDagger :: Quantifier -> String
resolveDagger Exists = "ff"
resolveDagger ForAll = "tt"

litVar :: Lit -> Int
litVar (Pos (Var j)) = j
litVar (Neg (Var j)) = j

expLit :: Lit -> String
expLit (Pos _) = "yes"
expLit (Neg _) = "no"

expDaggerLit :: Lit -> String
expDaggerLit (Pos _) = "no"
expDaggerLit (Neg _) = "yes"

queryLabel :: Int -> String
queryLabel q = "query_p" ++ show q

-- | Single forwarding branch: receive query_p[q] from right, forward to left,
-- receive response from left, forward back to right.
fwdBranch :: String -> String -> Int -> String -> String
fwdBranch left right q loop =
  queryLabel q ++ " : " ++ left ++ " ! { "
    ++ queryLabel q ++ " : " ++ left ++ " ? { "
      ++ "yes : " ++ right ++ " ! { yes : " ++ loop ++ " }, "
      ++ "no : " ++ right ++ " ! { no : " ++ loop ++ " }"
    ++ " } }"

-- | All forwarding branches for a set of variable indices.
fwdBranches :: String -> String -> [Int] -> String -> [String]
fwdBranches left right qs loop = [fwdBranch left right q loop | q <- qs]

-- | Select the k-th literal from a clause (1-based).
clauseLit :: Clause -> Int -> Lit
clauseLit (l1, _, _) 1 = l1
clauseLit (_, l2, _) 2 = l2
clauseLit (_, _, l3) 3 = l3
clauseLit _ _ = error "clauseLit: index out of range"

-- ---------------------------------------------------------------------------
-- T_s: controller that starts each evaluation round
-- ---------------------------------------------------------------------------

buildS :: String -> String
buildS tBad =
  "rec t . " ++ pName 1 ++ " ! [int]; "
    ++ pName 1 ++ " ? { tt : t, ff : " ++ tBad ++ " }"

-- ---------------------------------------------------------------------------
-- BuildVar(i): variable node encoding
-- ---------------------------------------------------------------------------

buildVar :: Int -> Int -> Quantifier -> String
buildVar n i qi =
  let left   = leftOfP i
      right  = rightOfP n i
      others = [1..i-1]  -- only forward for variables to the LEFT
      resQ   = resolve qi
      resDQ  = resolveDagger qi

      outerBranches =
        fwdBranches left right others "t0"
        ++ [ queryLabel i ++ " : " ++ right ++ " ! { no : t0 }" ]
        ++ [ resQ ++ " : " ++ left ++ " ! { " ++ resQ ++ " : t1 }" ]
        ++ [ resDQ ++ " : " ++ right ++ " ! [int]; "
             ++ "rec t1p . " ++ right ++ " ? { "
             ++ intercalate ", " innerBranches
             ++ " }" ]

      innerBranches =
        fwdBranches left right others "t1p"
        ++ [ queryLabel i ++ " : " ++ right ++ " ! { yes : t1p }" ]
        ++ [ "ff : " ++ left ++ " ! { ff : t1 }" ]
        ++ [ "tt : " ++ left ++ " ! { tt : t1 }" ]

  in "rec t1 . " ++ left ++ " ? [int]; " ++ right ++ " ! [int]; "
       ++ "rec t0 . " ++ right ++ " ? { "
       ++ intercalate ", " outerBranches
       ++ " }"

-- ---------------------------------------------------------------------------
-- BuildClause(i): clause-checking node
-- ---------------------------------------------------------------------------

buildClause :: Int -> Int -> Clause -> String
buildClause n i clause =
  let left  = leftOfR n i
      right = rightOfR i

      -- Fwds loop to t2 (inner receive), results loop to t (outer, new round)
      branches =
        fwdBranches left right [1..n] "t2"
        ++ [ "ff : " ++ left ++ " ! { ff : t }" ]
        ++ [ "tt : " ++ checkLiteral left clause 1 "t" ]

  in "rec t . " ++ left ++ " ? [int]; " ++ right ++ " ! [int]; "
       ++ "rec t2 . " ++ right ++ " ? { "
       ++ intercalate ", " branches
       ++ " }"

-- ---------------------------------------------------------------------------
-- CheckLiteral(i, k, loop): check k-th literal of clause i
-- ---------------------------------------------------------------------------

checkLiteral :: String -> Clause -> Int -> String -> String
checkLiteral left clause k loop =
  let lit = clauseLit clause k
      j   = litVar lit
      eL  = expLit lit
      eDL = expDaggerLit lit
      cont
        | k < 3     = checkLiteral left clause (k + 1) loop
        | otherwise  = left ++ " ! { ff : " ++ loop ++ " }"
  in left ++ " ! { " ++ queryLabel j ++ " : " ++ left ++ " ? { "
       ++ eL ++ " : " ++ left ++ " ! { tt : " ++ loop ++ " }, "
       ++ eDL ++ " : " ++ cont
       ++ " } }"

-- ---------------------------------------------------------------------------
-- T_r(m+1): base case (neutral element for conjunction, always tt)
-- ---------------------------------------------------------------------------

buildREnd :: Int -> Int -> String
buildREnd n m =
  let left = leftOfR n (m + 1)
  in "rec t . " ++ left ++ " ? [int]; " ++ left ++ " ! { tt : t }"

-- ---------------------------------------------------------------------------
-- Main encoder
-- ---------------------------------------------------------------------------

-- | Encode a QBF formula as a local context with a given T_bad.
-- The QBF is true iff the resulting context is live.
mkQBFWithBad :: String -> QBF -> LocalExample
mkQBFWithBad tBad (QBF quantifiers clauses) =
  let n = length quantifiers
      m = length clauses

      sType = ("s", buildS tBad)

      pTypes = [ (pName i, buildVar n i qi)
               | (i, (qi, _)) <- zip [1..] quantifiers ]

      rTypes = [ (rName i, buildClause n i clause)
               | (i, clause) <- zip [1..] clauses ]

      rEndType = (rName (m + 1), buildREnd n m)

  in LocalExample
    { leName = "QBF-" ++ show n ++ "v" ++ show m ++ "c"
    , leDisplayName = "QBF(" ++ show n ++ "v" ++ show m ++ "c)"
    , leCitation = Just udomYoshida2025
    , leParticipants = [sType] ++ pTypes ++ rTypes ++ [rEndType]
    }

-- | Encode a QBF formula with default T_bad = end.
mkQBF :: QBF -> LocalExample
mkQBF = mkQBFWithBad "end"

-- ---------------------------------------------------------------------------
-- Example formulas for verification
-- ---------------------------------------------------------------------------

-- | True: exists v1. (v1 \/ v1 \/ v1)
qbfTrue1 :: QBF
qbfTrue1 = QBF [(Exists, Var 1)]
  [(Pos (Var 1), Pos (Var 1), Pos (Var 1))]

-- | False: forall v1. (not v1 \/ not v1 \/ not v1)
-- When v1=1 the clause is false, so the universal fails.
qbfFalse1 :: QBF
qbfFalse1 = QBF [(ForAll, Var 1)]
  [(Neg (Var 1), Neg (Var 1), Neg (Var 1))]

-- | True: exists v1 forall v2. (v1 \/ v2 \/ v2) /\ (v1 \/ not v2 \/ not v2)
qbfTrue2 :: QBF
qbfTrue2 = QBF [(Exists, Var 1), (ForAll, Var 2)]
  [ (Pos (Var 1), Pos (Var 2), Pos (Var 2))
  , (Pos (Var 1), Neg (Var 2), Neg (Var 2))
  ]

-- | False: forall v1 forall v2. (v1 \/ v2 \/ v1) /\ (not v1 \/ not v2 \/ not v1)
qbfFalse2 :: QBF
qbfFalse2 = QBF [(ForAll, Var 1), (ForAll, Var 2)]
  [ (Pos (Var 1), Pos (Var 2), Pos (Var 1))
  , (Neg (Var 1), Neg (Var 2), Neg (Var 1))
  ]

-- | True: exists v1 forall v2 exists v3.
--     (v1 \/ v2 \/ not v3) /\ (not v1 \/ not v2 \/ not v3) /\ (not v1 \/ v2 \/ v3)
-- Three quantifier alternations. Winning strategy: v1=1, then v3=not v2.
-- The inner existential player must adapt to the universal player's choice.
qbfGame :: QBF
qbfGame = QBF
  [(Exists, Var 1), (ForAll, Var 2), (Exists, Var 3)]
  [ (Pos (Var 1), Pos (Var 2), Neg (Var 3))
  , (Neg (Var 1), Neg (Var 2), Neg (Var 3))
  , (Neg (Var 1), Pos (Var 2), Pos (Var 3))
  ]
