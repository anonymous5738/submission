-- | Convenience module re-exporting syntax AST, parser, pretty-printer,
-- and well-formedness checks.
module Syntax
  ( module Syntax.AST
  , module Syntax.AlphaEq
  , module Syntax.Parser
  , module Syntax.Pretty
  , module Syntax.WellFormed
  ) where

import Syntax.AST
import Syntax.AlphaEq
import Syntax.Parser
import Syntax.Pretty
import Syntax.WellFormed
