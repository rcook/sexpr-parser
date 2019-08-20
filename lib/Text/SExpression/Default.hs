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

module Text.SExpression.Default where

import Data.Semigroup (Last(..))
import Data.Default
import Text.SExpression.Types (SExpr(..), Parser)
import Control.Monad (void)
import Text.Megaparsec
    ( (<|>)
    , many
    , notFollowedBy
#ifdef MEGAPARSEC_7_OR_LATER
    , noneOf
#endif
    , some
    )
import Text.Megaparsec.Char
    ( char
    , digitChar
    , string
    , alphaNumChar
#ifndef MEGAPARSEC_7_OR_LATER
    , noneOf
#endif
    )

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

instance Semigroup LiteralParsersM where
  (<>)
    (LiteralParsersM ps pn pb)
    (LiteralParsersM ps' pn' pb') =
    LiteralParsersM (ps <> ps') (pn <> pn') (pb <> pb')

instance Default LiteralParsersM where
  def = LiteralParsersM
        { parseStringM = Just $ Last parseStringDef
        , parseNumberM = Just $ Last parseNumberDef
        , parseBoolM   = Just $ Last parseBoolDef
        }

instance Default LiteralParsers where
  def = mkLiteralParsers def

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
