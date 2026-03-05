module Benchmark.Size
  ( globalTypeSize
  , localTypeSize
  , contextSize
  ) where

import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Syntax.AST (Branches, GlobalType(..), LocalType(..), Participant)

-- | Count AST nodes in a global type.
globalTypeSize :: GlobalType -> Int
globalTypeSize GEnd = 1
globalTypeSize (GVar _) = 1
globalTypeSize (GRec _ body) = 1 + globalTypeSize body
globalTypeSize (GMessage _ _ branches) = 1 + branchesSize globalTypeSize branches

-- | Count AST nodes in a local type.
localTypeSize :: LocalType -> Int
localTypeSize LEnd = 1
localTypeSize (LVar _) = 1
localTypeSize (LRec _ body) = 1 + localTypeSize body
localTypeSize (LSend _ branches) = 1 + branchesSize localTypeSize branches
localTypeSize (LRecv _ branches) = 1 + branchesSize localTypeSize branches

-- | Sum of all local type sizes in a context.
contextSize :: Map.Map Participant LocalType -> Int
contextSize = foldl' (\acc lt -> acc + localTypeSize lt) 0

-- | Sum of (1 + size of continuation) for each branch.
branchesSize :: (a -> Int) -> Branches a -> Int
branchesSize sizeOf = foldl' (\acc (_, cont) -> acc + 1 + sizeOf cont) 0 . NE.toList
