{-# OPTIONS_GHC -Wall -Werror #-}

module Text.SExpression.InternalSpec (spec) where

import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Text.Megaparsec (parse)
import Text.SExpression.Internal
    ( parseAtom
    , parseDottedList
    , parseList
    , parseNumber
    , parseQuoted
    , parseSExpr
    , parseString
    )
import Text.SExpression.Types (SExpr(..))

spec :: Spec
spec = do
    describe "parseSExpr" $
        it "parses test expressions" $ do
            parse parseSExpr "" "assert" `shouldBe` Right (Atom "assert")
            parse parseSExpr "" "(assert)" `shouldBe` Right (List [Atom "assert"])
            parse parseSExpr "" "(assert (and))" `shouldBe` Right (List [Atom "assert", List [Atom "and"]])
            parse parseSExpr "" "(assert (and (>)))" `shouldBe` Right (List [Atom "assert", List [Atom "and", List [Atom ">"]]])
            parse parseSExpr "" "(assert (and (> (* 2 a) (+ b c)) (> (* 2 b) (+ c d))))" `shouldBe`
                Right (List
                        [ Atom "assert"
                        , List
                            [ Atom "and"
                            , List
                                [ Atom ">"
                                , List
                                    [ Atom "*"
                                    , Number 2
                                    , Atom "a"
                                    ]
                                , List
                                    [ Atom "+"
                                    , Atom "b"
                                    , Atom "c"
                                    ]
                                ]
                            , List
                                [ Atom ">"
                                , List
                                    [ Atom "*"
                                    , Number 2
                                    , Atom "b"
                                    ]
                                , List
                                    [ Atom "+"
                                    , Atom "c"
                                    , Atom "d"
                                    ]
                                ]
                            ]
                        ])
    describe "parseAtom" $ do
        it "parses #t" $
            parse parseAtom "" "#t" `shouldBe` Right (Bool True)
        it "parses #f" $
            parse parseAtom "" "#f" `shouldBe` Right (Bool False)
        it "parses single-character atom" $
            parse parseAtom "" "x" `shouldBe` Right (Atom "x")
        it "parses other atom" $
            parse parseAtom "" "foo" `shouldBe` Right (Atom "foo")
    describe "parseList" $
        it "parses a list of different atoms" $
            parse parseList "" "#t #f x foo 123" `shouldBe` Right (List [Bool True, Bool False, Atom "x", Atom "foo", Number 123])
    describe "parseDottedList" $
        it "parses a dotted list" $
            parse parseDottedList "" "123 . foo" `shouldBe` Right (DottedList [Number 123] (Atom "foo"))
    describe "parseNumber" $
        it "parses number literal" $
            parse parseNumber "" "123" `shouldBe` Right (Number 123)
    describe "parseString" $
        it "parses string literal" $
            parse parseString "" "\"foo\"" `shouldBe` Right (String "foo")
    describe "parseQuoted" $
        it "parses quoted expression" $
            parse parseQuoted "" "'\"foo\"" `shouldBe` Right (List [Atom "quote", String "foo"])
    describe "parseSExpr" $ do
        it "parses Z3 output" $ do
            let Right result = parse parseSExpr ""
                                "(model\n\
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
            result `shouldBe`
                List
                    [ Atom "model"
                    , List
                        [ Atom "define-fun"
                        , Atom "a4"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "c3"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "d4"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "a2"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "c5"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "b4"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "b5"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "b2"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "d1"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "d2"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "b1"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "a5"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "c2"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "d3"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "c4"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "a1"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "d5"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "b3"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "c0"
                        , List []
                        , Atom "bool"
                        , Atom "false"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "c1"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    , List
                        [ Atom "define-fun"
                        , Atom "a3"
                        , List []
                        , Atom "bool"
                        , Atom "true"
                        ]
                    ]
