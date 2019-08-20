{-|
Module      : Text.SExpression.Internal
Description : Internal parser functions
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides internal parser functions.
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#undef MEGAPARSEC_7_OR_LATER
#ifdef MIN_VERSION_GLASGOW_HASKELL
-- GHC >= 7.10.1.0
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- GHC >= 8.0.0.0
#if MIN_VERSION_megaparsec(7,0,0)
#define MEGAPARSEC_7_OR_LATER
#endif
#endif
#endif

module Text.SExpression.Internal
    ( -- * S-expression parser
      parseSExpr
    , -- * S-expression value parsers
      parseAtom
    , parseConsList
    , parseList
    , parseQuoted
    , parseStringDef
    , parseNumberDef
    , parseBoolDef
    , mkLiteralParsers
    , overrideBoolP
    , overrideNumberP
    , overrideStringP
    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
    ( (<|>)
    , endBy
    , many
#ifdef MEGAPARSEC_7_OR_LATER
    , oneOf
#endif
    , sepBy
    , try
    )
import Text.Megaparsec.Char
    ( char
    , digitChar
    , letterChar
#ifndef MEGAPARSEC_7_OR_LATER
    , oneOf
#endif
    , space1
    )
import Text.Megaparsec.Char.Lexer
    ( space
    , skipLineComment
    )
import Text.SExpression.Types (Parser, SExpr(..))
import Text.SExpression.Default

sc :: Parser ()
sc = space space1 lineComment empty
    where
        lineComment = skipLineComment ";"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

-- | S-expression parser
parseSExpr ::
    LiteralParsers ->
    Parser SExpr    -- ^ parser
parseSExpr lp@(LiteralParsers{..}) =
    try parseBool
    <|> parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted lp
    <|> do
            void $ char '('
            lst <- (try $ parseList lp) <|> parseConsList lp
            void $ char ')' >> sc
            pure lst

-- | Parse s-expression atom
parseAtom ::
    Parser SExpr    -- ^ parser
parseAtom = do
    h <- letterChar <|> symbol
    t <- many (letterChar <|> digitChar <|> symbol)
    return . Atom $ h : t


-- | Parse s-expression list
parseList ::
    LiteralParsers ->
    Parser SExpr    -- ^ parser
parseList lp =
    List <$> parseSExpr lp `sepBy` sc

-- | Parse s-expression cons list
parseConsList ::
    LiteralParsers ->
    Parser SExpr    -- ^ parser
parseConsList lp = do
    h <- parseSExpr lp `endBy` sc
    t <- char '.' >> sc >> parseSExpr lp
    pure $ ConsList h t

-- | Parse s-expression quoted expression
parseQuoted ::
    LiteralParsers ->
    Parser SExpr    -- ^ parser
parseQuoted lp = do
    void $ char '\''
    e <- parseSExpr lp
    pure $ List [Atom "quote", e]
