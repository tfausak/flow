<p align="center">
    <img alt="" src="https://a.pomf.se/gnowsh.svg">
</p>

<h1 align="center">
    <a href="https://github.com/tfausak/flow">
        Flow
    </a>
</h1>

<p align="center">
    Write more understandable Haskell.
</p>

<p align="center">
    <a href="https://hackage.haskell.org/package/flow"><img alt="" src="https://img.shields.io/hackage/v/flow.svg?label=version&style=flat-square"></a>
    <a href="https://travis-ci.org/tfausak/flow"><img alt="" src="https://img.shields.io/travis/tfausak/flow/master.svg?label=build&style=flat-square"></a>
    <a href="http://packdeps.haskellers.com/feed?needle=flow"><img alt="" src="https://img.shields.io/hackage-deps/v/flow.svg?label=dependencies&style=flat-square"></a>
</p>

<hr>

Flow is a package that provides functions and operators for writing more
understandable Haskell. It's an alternative to some common idioms like function
application with [`($)`][] and function composition with [`(.)`][].

-   [Requirements](#requirements)
-   [Installation](#installation)
-   [Usage](#usage)

## Requirements

Flow requires a Haskell compiler. It is tested with recent versions of GHC, but
older or different compilers should be acceptable. For installation with Cabal,
Flow requires at least Cabal 1.8.

## Installation

To add Flow as a dependency to your package, add it to your Cabal file.

```
build-depends: flow ==1.*
```

For other use cases, install it with Cabal.

``` sh
$ cabal install 'flow ==1.*'
```

Flow uses [Semantic Versioning][]. See [the change log][] for a detailed list
of changes.

## Usage

Flow is designed to be imported unqualified. It does not export anything that
conflicts with [the base package][].

``` hs
import Flow
```

Here is a quick overview of the functions and operators that Flow provides.

Base          | Flow
------------- | ---------------
`f x`         | `apply x f`
`x & f`       | `x |> f`
`f $ x`       | `f <| x`
`g (f x)`     | `compose f g x`
`g >>> f`     | `f .> g`
`g . f`       | `g <. f`
`seq x (f x)` | `apply' x f`
`f $! x`      | `x !> f`
`f $! x`      | `f <! x`

For more information about Flow, please read [the Haddock documentation][].

[`($)`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:-36-
[`(.)`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[semantic versioning]: http://semver.org/spec/v2.0.0.html
[the change log]: CHANGELOG.md
[the base package]: http://hackage.haskell.org/package/base
[the haddock documentation]: https://hackage.haskell.org/package/flow/docs/Flow.html
