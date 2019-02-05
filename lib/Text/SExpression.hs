{-|
Module      : Text.SExpression
Description : S-expression parser
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides a 'parseSExpr' function which parses simple
s-expressions represented using the 'SExpr' type from 'String' input.
-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Text.SExpression
    ( -- * Parser context
      Parser
    , -- * S-expression values
      SExpr(..)
    , -- * S-expression parser
      parseSExpr
    ) where

import Text.SExpression.Internal (parseSExpr)
import Text.SExpression.Types (Parser, SExpr(..))
