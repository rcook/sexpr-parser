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

parseSExpr :: Parser SExpr
parseSExpr =
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
            void $ char '('
            lst <- (try parseList) <|> parseDottedList
            void $ char ')' >> sc
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
parseList = List <$> parseSExpr `sepBy` sc

parseDottedList :: Parser SExpr
parseDottedList = do
    h <- parseSExpr `endBy` sc
    t <- char '.' >> sc >> parseSExpr
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
