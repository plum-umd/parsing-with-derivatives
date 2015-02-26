-- As is standard practice, DerParser is a single module that combines all
-- DerParser.* modules. A very short description of each module is given.

module DerParser

-- DerParser.Builder contains all the smart constructors and combinator functions
-- for building parsers.

  ( module DerParser.Builder

-- DerParser.Derivative contains the rules for computing the derivative of a
-- parser.

  , module DerParser.Derivative
--
--  , module DerParser.Dotify

-- DerParser.FPMaybe contains the data structure FPMaybe.  It does not contain
-- anything relating to parsing.

 , module DerParser.FPMaybe

-- DerParser.FixPoint contains the rules for computing the fixed point of the
-- metadata for a parser.

  , module DerParser.FixMeta

-- DerParser.Knot contains two Type Classes, Knot and DeriveKnot.  These type
-- classes are not just for convenience, they are necessary for being able to
-- express this entire implementation accross multiple modules (otherwise
-- everything would need to be in a single file.)  More details on the reasons
-- inside.

--  , module DerParser.Knot

-- DerParser.KnotImpl contains the Parser instances of Knot and DeriveKnot.

--  , module DerParser.KnotImpl

-- DerParser.MemoMap contains an impure, naive function memoizer.

--  , module DerParser.MemoMap

-- DerParser.Parser contains the Parser data structure.

  , module DerParser.Parser

  , module DerParser.Reify

  , module DerParser.ResultType
  -- , module DerParser.SetDerivative
  , module DerParser.Stock
  , module DerParser.Token

-- DerParser.Weed contains the rules for weeding a parser.  Weeding is the process
-- of removing areas of the graph that are superfulous, resulting in a smaller but
-- semantically identical parser.

  , module DerParser.Weed

-- DerParser.Zip contains the rules for how parsing LL1 grammars can be done in
-- linear time; my latest contribution to this theory of parsing.

  , module DerParser.Zip

  ) where

import DerParser.Builder
import DerParser.Derivative
-- import DerParser.Dotify
import DerParser.FPMaybe
import DerParser.FixMeta
-- import DerParser.Knot
-- import DerParser.KnotImpl
-- import DerParser.MemoMap
import DerParser.Parser
import DerParser.Reify
import DerParser.ResultType
-- import DerParser.SetDerivative
import DerParser.Stock
import DerParser.Token
import DerParser.Weed
import DerParser.Zip
