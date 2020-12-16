hcount
===

[![Build Status](https://travis-ci.org/tonyday567/hcount.svg)](https://travis-ci.org/tonyday567/hcount) [![Hackage](https://img.shields.io/hackage/v/hcount.svg)](https://hackage.haskell.org/package/hcount)

Counting of Haskell names and artifacts usage, based on stan.

Usage
===

- Include the following ghc-options in the cabal file of the libraries you want to analyse:

```
-fwrite-ide-info
-hiedir=.hie
```
- Install hcount

```
stack install hcount
```

- run hcount

```
cd project
hcount
```

See 'hcount --help' for options.

develop
----

```
stack exec "hcount" --file-watch
```

