# sexpr-parser

S-expression parser in Haskell

## Description

This is a [Megaparsec][megaparsec]-based parser for s-expression, heavily inspired by [lispparser][lispparser]. I'm building this to parse [SMT-LIB v2][smt-lib] output. The [sample program](src/Main.hs) does exactly that, so there.

## Run demo

This demo assumes that you have [Z3][z3] installed and available on your system search path:

```bash
stack build --fast --exec sexpr-parser-z3-demo
```

## Licence

[MIT License](LICENSE)

[lispparser]: http://hackage.haskell.org/package/lispparser
[megaparsec]: http://hackage.haskell.org/package/megaparsec
[smt-lib]: http://smtlib.cs.uiowa.edu/language.shtml
[z3]: https://github.com/Z3Prover/z3
