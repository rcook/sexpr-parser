{-|
Module      : Text.SExpression.Types
Description : Types
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides parser context type 'Parser' and value type 'SExpr'.
-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Text.SExpression.Types
    ( Parser
    , SExpr(..)
    ) where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

-- | Parser context
type Parser = Parsec Void String

-- | S-expression values
data SExpr =
    Atom String                 -- ^ atom
    | List [SExpr]              -- ^ list
    | ConsList [SExpr] SExpr    -- ^ cons list
    | Number Integer            -- ^ number literal
    | String String             -- ^ string literal
    | Bool Bool                 -- ^ Boolean literal
    deriving (Eq, Read, Show)
