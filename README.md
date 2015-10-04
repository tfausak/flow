# [Flow][]

Write more understandable Haskell.

[![Version][]](https://hackage.haskell.org/package/flow)
[![Build][]](https://travis-ci.org/tfausak/flow)
[![Dependencies][]](http://packdeps.haskellers.com/feed?needle=flow)

---

Flow is a package that provides functions and operators for writing more
understandable Haskell. It is an alternative to some common idioms like
[`($)`][] for function application and [`(.)`][] for function composition.

-   [Requirements](#requirements)
-   [Installation](#installation)
-   [Usage](#usage)
    -   [Cheat sheet](#cheat-sheet)

## Requirements

Flow requires a Haskell compiler. It is tested with recent versions of GHC, but
older or different compilers should be acceptable. For installation with Cabal,
Flow requires at least Cabal 1.8.

## Installation

To add Flow as a dependency to your package, add it to your Cabal file.

```
build-depends: flow ==1.0.*
```

See [the change log][] for a detailed list of changes.

## Usage

Flow is designed to be imported unqualified. It does not export anything that
conflicts with [the base package][].

``` hs
import Flow
```

### Cheat sheet

Flow            | Base
--------------- | -------------
`x |> f`        | `x & f`
`f <| x`        | `f $ x`
`apply x f`     | `f x`
`f .> g`        | `f >>> g`
`g <. f`        | `g . f`
`compose f g x` | `g (f x)`
`x !> f`        | -
`f <! x`        | `f $! x`
`apply' x f`    | `seq x (f x)`

For more information about Flow, please read [the Haddock documentation][].

[flow]: http://taylor.fausak.me/flow/
[version]: https://img.shields.io/hackage/v/flow.svg?label=version
[build]: https://img.shields.io/travis/tfausak/flow/master.svg?label=build
[dependencies]: https://img.shields.io/hackage-deps/v/flow.svg?label=dependencies
[`($)`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:-36-
[`(.)`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[the change log]: CHANGELOG.md
[the base package]: http://hackage.haskell.org/package/base
[the haddock documentation]: https://hackage.haskell.org/package/flow/docs/Flow.html
