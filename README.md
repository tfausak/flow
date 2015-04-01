<h1 align="center">
    <a href="https://github.com/tfausak/flow">
        Flow
    </a>
</h1>

<p align="center">
    Functions and operators for more understandable Haskell
</p>

<p align="center">
    <a href="https://hackage.haskell.org/package/flow"><img alt="" src="https://img.shields.io/hackage/v/flow.svg"></a>
    <a href="https://travis-ci.org/tfausak/flow"><img alt="" src="https://img.shields.io/travis/tfausak/flow/master.svg"></a>
    <a href="http://packdeps.haskellers.com/feed?needle=flow"><img alt="" src="https://img.shields.io/hackage-deps/v/flow.svg"></a>
</p>

<hr>

Flow provides functions and operators for writing more understandable Haskell.

-   [Install](#install)
-   [Use](#use)
-   [Develop](#develop)

## Install

To use Flow in a Cabal package, add it to your Cabal file.

```
build-depends:
    flow ==0.0.*
```

For other use cases, install it with Cabal.

``` sh
$ cabal update
$ cabal install 'flow ==0.0.*'
```

Flow uses [Semantic Versioning][]. Check out [the change log][] for a
detailed list of changes.

## Use

Flow is designed to be imported unqualified. It does not export anything
that conflicts with the Prelude. To get started, simply import it.

``` hs
import Flow
```

Check out [the Haddock documentation][] for more information about the
functions Flow provides.

## Develop

If you want to help develop Flow, you'll need Git, GHC, and Cabal. To get
started, clone the repository and install the dependencies.

``` sh
$ git clone https://github.com/tfausak/flow
$ cd flow

$ cabal sandbox init
$ cabal install --enable-benchmarks --enable-tests --only-dependencies
```

Once you've done that, you should be able to use the normal Cabal tools
(`repl`, `test`, `haddock`, and `bench` in particular). If you've made changes
that you want merged into this repository, create a fork and open a pull
request. GitHub's [Fork A Repo][] article can help with that.

[semantic versioning]: http://semver.org/spec/v2.0.0.html
[the change log]: CHANGELOG.md
[the haddock documentation]: https://hackage.haskell.org/package/flow
[fork a repo]: https://help.github.com/articles/fork-a-repo/
