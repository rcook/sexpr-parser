{-# OPTIONS_GHC -Wall -Werror #-}

module Main (main) where

import Data.Foldable (for_)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.Megaparsec (parse, someTill)
import Text.Megaparsec.Char (char, lowerChar)
import Text.Printf (printf)
import Text.SExpression (Parser, SExpr(..), parseSExpr)

data Z3Result = Satisfied | Unsatisfied deriving Show

data Z3Output = Z3Output Z3Result SExpr deriving Show

parseZ3Result :: Parser Z3Result
parseZ3Result = do
    s <- someTill lowerChar (char '\n')
    case s of
        "sat" -> pure Satisfied
        "unsat" -> pure Unsatisfied
        _ -> fail $ printf "Unexpected result: %s" s

parseZ3Output :: Parser Z3Output
parseZ3Output = Z3Output <$> parseZ3Result <*> parseSExpr

z3SatOutput :: String
z3SatOutput = "sat\n\
    \(model\n\
    \(define-fun a4 () bool\n\
    \  false)\n\
    \(define-fun c3 () bool\n\
    \  true)\n\
    \(define-fun d4 () bool\n\
    \  false)\n\
    \(define-fun a2 () bool\n\
    \  true)\n\
    \(define-fun c5 () bool\n\
    \  false)\n\
    \(define-fun b4 () bool\n\
    \  true)\n\
    \(define-fun b5 () bool\n\
    \  true)\n\
    \(define-fun b2 () bool\n\
    \  false)\n\
    \(define-fun d1 () bool\n\
    \  true)\n\
    \(define-fun d2 () bool\n\
    \  false)\n\
    \(define-fun b1 () bool\n\
    \  false)\n\
    \(define-fun a5 () bool\n\
    \  true)\n\
    \(define-fun c2 () bool\n\
    \  true)\n\
    \(define-fun d3 () bool\n\
    \  true)\n\
    \(define-fun c4 () bool\n\
    \  true)\n\
    \(define-fun a1 () bool\n\
    \  false)\n\
    \(define-fun d5 () bool\n\
    \  false)\n\
    \(define-fun b3 () bool\n\
    \  true)\n\
    \(define-fun c0 () bool\n\
    \  false)\n\
    \(define-fun c1 () bool\n\
    \  true)\n\
    \(define-fun a3 () bool\n\
    \  true))"

main :: IO ()
main = do
    let Right (Z3Output result f) = parse parseZ3Output "" z3SatOutput
    for_ (sort (boolFuns f)) $ \(name, value) ->
        putStrLn $ printf "%s = %s" name (if value then "1" else "0")
    putStrLn $ "result=" ++ show result

boolFuns :: SExpr -> [(String, Bool)]
boolFuns (List (Atom "model" : fs)) = catMaybes $ map p fs
    where
        p :: SExpr -> Maybe (String, Bool)
        p (List [Atom "define-fun", Atom name, List [], Atom "bool", Atom "false"]) = Just (name, False)
        p (List [Atom "define-fun", Atom name, List [], Atom "bool", Atom "true"]) = Just (name, True)
        p _ = Nothing
boolFuns _ = []
