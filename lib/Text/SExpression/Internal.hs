{-# OPTIONS_GHC -Wall -Werror #-}

module Text.SExpression.Internal
    ( parseAtom
    , parseDottedList
    , parseList
    , parseNumber
    , parseQuoted
    , parseSExpr
    , parseString
    ) where

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
    , space
    )
import Text.SExpression.Types (Parser, SExpr(..))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

parseSExpr :: Parser SExpr
parseSExpr =
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
            void $ char '('
            lst <- (try parseList) <|> parseDottedList
            void $ char ')'
            pure lst

parseAtom :: Parser SExpr
parseAtom = do
    h <- letterChar <|> symbol
    t <- many (letterChar <|> digitChar <|> symbol)
    let s = h : t
    pure $ case s of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom s

parseList :: Parser SExpr
parseList = List <$> parseSExpr `sepBy` space

parseDottedList :: Parser SExpr
parseDottedList = do
    h <- parseSExpr `endBy` space
    t <- char '.' >> space >> parseSExpr
    pure $ DottedList h t

parseNumber :: Parser SExpr
parseNumber = (Number . read) <$> some digitChar

parseString :: Parser SExpr
parseString = do
    void $ char '"'
    s <- many (noneOf "\"")
    void $ char '"'
    pure $ String s

parseQuoted :: Parser SExpr
parseQuoted = do
    void $ char '\''
    e <- parseSExpr
    pure $ List [Atom "quote", e]
