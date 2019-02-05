{-# OPTIONS_GHC -Wall -Werror #-}

module Text.SExpression
    ( Parser
    , SExpr(..)
    , parseSExpr
    ) where

import Text.SExpression.Internal (parseSExpr)
import Text.SExpression.Types (Parser, SExpr(..))
