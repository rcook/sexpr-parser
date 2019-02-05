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

module Text.SExpression.Internal
    ( -- * S-expression parser
      parseSExpr
    , -- * S-expression value parsers
      parseAtom
    , parseConsList
    , parseList
    , parseNumber
    , parseQuoted
    , parseString
    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
    ( (<|>)
    , endBy
    , many
    , sepBy
    , some
    , try
    )
import Text.Megaparsec.Char
    ( char
    , digitChar
    , letterChar
    , noneOf
    , oneOf
    , space1
    )
import Text.Megaparsec.Char.Lexer
    ( space
    , skipLineComment
    )
import Text.SExpression.Types (Parser, SExpr(..))

sc :: Parser ()
sc = space space1 lineComment empty
    where
        lineComment = skipLineComment ";"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

-- | S-expression parser
parseSExpr ::
    Parser SExpr    -- ^ parser
parseSExpr =
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
            void $ char '('
            lst <- (try parseList) <|> parseConsList
            void $ char ')' >> sc
            pure lst

-- | Parse s-expression atom
parseAtom ::
    Parser SExpr    -- ^ parser
parseAtom = do
    h <- letterChar <|> symbol
    t <- many (letterChar <|> digitChar <|> symbol)
    let s = h : t
    pure $ case s of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom s

-- | Parse s-expression list
parseList ::
    Parser SExpr    -- ^ parser
parseList = List <$> parseSExpr `sepBy` sc

-- | Parse s-expression cons list
parseConsList ::
    Parser SExpr    -- ^ parser
parseConsList = do
    h <- parseSExpr `endBy` sc
    t <- char '.' >> sc >> parseSExpr
    pure $ ConsList h t

-- | Parse s-expression number literal
parseNumber ::
    Parser SExpr    -- ^ parser
parseNumber = (Number . read) <$> some digitChar

-- | Parse s-expression string literal
parseString ::
    Parser SExpr    -- ^ parser
parseString = do
    void $ char '"'
    s <- many (noneOf "\"")
    void $ char '"'
    pure $ String s

-- | Parse s-expression quoted expression
parseQuoted ::
    Parser SExpr    -- ^ parser
parseQuoted = do
    void $ char '\''
    e <- parseSExpr
    pure $ List [Atom "quote", e]
