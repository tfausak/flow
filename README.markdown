# [Flow][]

[![Workflow](https://github.com/tfausak/flow/actions/workflows/workflow.yaml/badge.svg)](https://github.com/tfausak/flow/actions/workflows/workflow.yaml)
[![Hackage](https://img.shields.io/hackage/v/flow)](https://hackage.haskell.org/package/flow)
[![Stackage](https://www.stackage.org/package/flow/badge/nightly?label=stackage)](https://www.stackage.org/package/flow)

Write more understandable Haskell.

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
Flow requires at least Cabal 2.2.

## Installation

To add Flow as a dependency to your package, add it to your Cabal file.

```
build-depends: flow ==2.0.*
```

## Usage

Flow is designed to be imported unqualified. It does not export anything that
conflicts with [the base package][].

``` hs
import Flow
```

### Cheat sheet

Flow            | Base
--------------- | -------------
<code>x &#124;> f</code> | `x & f`
<code>f <&#124; x</code> | `f $ x`
`apply x f`     | `f x`
`f .> g`        | `f >>> g`
`g <. f`        | `g . f`
`compose f g x` | `g (f x)`
`x !> f`        | -
`f <! x`        | `f $! x`
`apply' x f`    | `seq x (f x)`

### Converting existing code

If you want to uniformly use flow operators you can use [HLint] with the
`hlint-flow.yaml` file. For easy use, it is best to extract the
`hlint-flow.yaml` to your project directory and do

``` sh
> hlint -h hlint-flow.yaml <source file>
```

or

``` sh
> hlint --git -h hlint-flow.yaml
```

to check all Haskell source tracked by git.

For more information about Flow, please read [the Haddock documentation][].

[HLint]: https://github.com/ndmitchell/hlint
[Flow]: http://taylor.fausak.me/flow/
[`($)`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:-36-
[`(.)`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[the base package]: http://hackage.haskell.org/package/base
[the haddock documentation]: https://hackage.haskell.org/package/flow/docs/Flow.html
