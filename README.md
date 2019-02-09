# sexpr-parser

[![Travis CI](https://img.shields.io/travis/rcook/sexpr-parser/master.svg)](https://travis-ci.org/rcook/sexpr-parser)
[![Hackage](https://img.shields.io/hackage/v/sexpr-parser.svg)](http://hackage.haskell.org/package/sexpr-parser)
[![Hackage dependencies](https://img.shields.io/hackage-deps/v/sexpr-parser.svg)](http://hackage.haskell.org/package/sexpr-parser)
[![Licence](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rcook/sexpr-parser/master/LICENSE)

S-expression parser in Haskell

## Description

This is a [Megaparsec][megaparsec]-based parser for s-expressions, heavily inspired by [lispparser][lispparser]. I'm building this to parse [SMT-LIB v2][smt-lib] output from [Z3][z3], so there.

## Run Z3 demo

The [Z3 demo](z3-demo/Main.hs) parses SMT-LIB v2 output from Z3. The executable assumes that you have Z3 installed and the `z3` executable available on your system search path:

```bash
stack build --fast --exec sexpr-parser-z3-demo
```

## Licence

[MIT License](LICENSE)

[lispparser]: http://hackage.haskell.org/package/lispparser
[megaparsec]: http://hackage.haskell.org/package/megaparsec
[smt-lib]: http://smtlib.cs.uiowa.edu/language.shtml
[z3]: https://github.com/Z3Prover/z3
