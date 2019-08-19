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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

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
    , def
    ) where

import Data.Default (Default(..))
import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
    ( (<|>)
    , endBy
    , many
    , notFollowedBy
#ifdef MEGAPARSEC_7_OR_LATER
    , noneOf
    , oneOf
#endif
    , sepBy
    , some
    , try
    )
import Text.Megaparsec.Char
    ( char
    , digitChar
    , letterChar
    , string
    , alphaNumChar
#ifndef MEGAPARSEC_7_OR_LATER
    , noneOf
    , oneOf
#endif
    , space1
    )
import Text.Megaparsec.Char.Lexer
    ( space
    , skipLineComment
    )
import Text.SExpression.Types (Parser, SExpr(..))
import Data.Semigroup (Last(..))

data LiteralParsersM = LiteralParsersM
  { parseStringM :: Maybe (Last (Parser SExpr))
  , parseNumberM :: Maybe (Last (Parser SExpr))
  , parseBoolM   :: Maybe (Last (Parser SExpr))
  }

data LiteralParsers = LiteralParsers
  { parseString :: Parser SExpr
  , parseNumber :: Parser SExpr
  , parseBool   :: Parser SExpr
  }

mkLiteralParsers ::
  (LiteralParsersM -> LiteralParsersM) ->
  LiteralParsers
mkLiteralParsers f =
  case f def of
    LiteralParsersM{..} ->
      let Just (Last parseString) = parseStringM
          Just (Last parseNumber) = parseNumberM
          Just (Last parseBool)   = parseBoolM in
        LiteralParsers parseString parseNumber parseBool

overrideStringP ::
  Parser SExpr -> LiteralParsersM ->  LiteralParsersM
overrideStringP sp lp = lp <>
  LiteralParsersM
  { parseStringM = Just $ Last sp
  , parseNumberM = Nothing
  , parseBoolM   = Nothing
  }

overrideNumberP ::
  Parser SExpr -> LiteralParsersM ->  LiteralParsersM
overrideNumberP np lp = lp <>
  LiteralParsersM
  { parseStringM = Nothing
  , parseNumberM = Just $ Last np
  , parseBoolM   = Nothing
  }

overrideBoolP ::
  Parser SExpr -> LiteralParsersM ->  LiteralParsersM
overrideBoolP bp lp = lp <>
  LiteralParsersM
  { parseStringM = Nothing
  , parseNumberM = Nothing
  , parseBoolM   = Just $ Last bp
  }
  
instance Semigroup LiteralParsersM where
  (<>)
    (LiteralParsersM ps pn pb)
    (LiteralParsersM ps' pn' pb') =
    LiteralParsersM (ps <> ps') (pn <> pn') (pb <> pb')

instance Monoid LiteralParsersM where
  mempty = LiteralParsersM Nothing Nothing Nothing

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

-- | Default parser for s-expression boolean literals
parseBoolDef ::
  Parser SExpr
parseBoolDef = do
  b <-  string "#t" <* notFollowedBy alphaNumChar
    <|> string "#f" <* notFollowedBy alphaNumChar
  case b of
    "#t" -> return $ Bool True
    "#f" -> return $ Bool False
    _ -> fail "Not a boolean"
  
-- | Default parser for s-expression numeric literals
parseNumberDef ::
    Parser SExpr    -- ^ parser
parseNumberDef = (Number . read) <$> some digitChar

-- | Default parser for s-expression string literals
parseStringDef ::
    Parser SExpr    -- ^ parser
parseStringDef = do
    void $ char '"'
    s <- many (noneOf "\"")
    void $ char '"'
    pure $ String s

instance Default LiteralParsersM where
  def = LiteralParsersM
        { parseStringM = Just $ Last parseStringDef
        , parseNumberM = Just $ Last parseNumberDef
        , parseBoolM   = Just $ Last parseBoolDef
        }

instance {-# OVERLAPPING #-} Default
  (LiteralParsersM -> LiteralParsersM) where
  def = id

instance Default LiteralParsers where
  def = mkLiteralParsers def

-- | Parse s-expression quoted expression
parseQuoted ::
    LiteralParsers ->
    Parser SExpr    -- ^ parser
parseQuoted lp = do
    void $ char '\''
    e <- parseSExpr lp
    pure $ List [Atom "quote", e]
