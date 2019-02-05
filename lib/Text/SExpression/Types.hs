{-# OPTIONS_GHC -Wall -Werror #-}

module Text.SExpression.Types
    ( Parser
    , SExpr(..)
    ) where

import Data.Void
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

data SExpr =
    Atom String
    | List [SExpr]
    | DottedList [SExpr] SExpr
    | Number Integer
    | String String
    | Bool Bool
    deriving (Eq, Read, Show)
