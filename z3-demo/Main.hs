{-# OPTIONS_GHC -Wall -Werror #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Maybe (catMaybes)
import System.IO (BufferMode(..), hGetContents, hPutStrLn, hSetBuffering)
import System.Process
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, string)
import Text.Printf (printf)
import Text.SExpression (Parser, SExpr(..), parseSExpr)

data Z3SATResult = Satisfied | Unsatisfied deriving Show

data Z3Output = Z3Output Z3SATResult SExpr deriving Show

main :: IO ()
main = do
    result <- checkSATWithZ3 "input.smt2" $
        "(push)\n\
        \(declare-const x bool)\n\
        \(declare-const y bool)\n\
        \(assert (and (not x) y))\n\
        \(check-sat)\n\
        \(get-model)\n\
        \(pop)\n\
        \(exit)\n"
    case result of
        Left e -> putStrLn $ "Error: " ++ e
        Right (satResult, funs) -> do
            for_ funs $ \(name, value) ->
                putStrLn $ printf "%s = %s" name (if value then "1" else "0")
            putStrLn $ "result=" ++ show satResult

parseZ3SATResult :: Parser Z3SATResult
parseZ3SATResult = do
    s <- string "sat" <|> string "unsat"
    void $ char '\n'
    case s of
        "sat" -> pure Satisfied
        "unsat" -> pure Unsatisfied
        _ -> error "Unreachable"

parseZ3Output :: Parser Z3Output
parseZ3Output = Z3Output <$> parseZ3SATResult <*> parseSExpr

checkSATWithZ3 :: String -> String -> IO (Either String (Z3SATResult, [(String, Bool)]))
checkSATWithZ3 ctx input = do
    output <- withCreateProcess (proc "z3" ["-in"])
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = Inherit
                        } $ \(Just hIn) (Just hOut) _ _ -> do
        hSetBuffering hIn NoBuffering
        hPutStrLn hIn input
        s <- hGetContents hOut
        void $ evaluate (length s)
        pure s
    case parse parseZ3Output ctx output of
        Left e -> pure $ Left (show e)
        Right (Z3Output satResult f) -> pure $ Right (satResult, sort (boolFuns f))

boolFuns :: SExpr -> [(String, Bool)]
boolFuns (List (Atom "model" : fs)) = catMaybes $ map p fs
    where
        p :: SExpr -> Maybe (String, Bool)
        p (List [Atom "define-fun", Atom name, List [], Atom "bool", Atom "false"]) = Just (name, False)
        p (List [Atom "define-fun", Atom name, List [], Atom "bool", Atom "true"]) = Just (name, True)
        p _ = Nothing
boolFuns _ = []
