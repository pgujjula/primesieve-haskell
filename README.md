# `primesieve` for Haskell
[![Haskell-CI](https://github.com/pgujjula/primesieve-haskell/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/pgujjula/primesieve-haskell/actions/workflows/haskell-ci.yml)

This library provides Haskell bindings to Kim Walisch's
[primesieve](https://github.com/kimwalisch/primesieve) library.

### Build instructions
First follow the
[installation directions](https://github.com/kimwalisch/primesieve#installation)
for the original library. As stated in the directions, make sure to install the
development version of the original library. Then you can build this library
with stack or cabal and read the documentation.
```
# stack
stack build
stack test
stack haddock primesieve --open

# cabal
cabal update
cabal build
cabal test
cabal haddock  # and then open the documentation manually
```

### Bugs
Report any bugs on the Github issue tracker, or by emailing
libraries@mail.preetham.io
