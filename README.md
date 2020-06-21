## Chromar Language and Simulator

![cabal](https://github.com/BlockScope/Chromar/workflows/cabal/badge.svg)

### Build Tools

We show how to get going with both stack and cabal. Pick either one or both.

Stack will install a compiler version and pick dependencies that are known to
work together. These are published on
[stackage.org](https://www.stackage.org/). We've picked a resolver in
[`./stack.yaml`](/stack.yaml).

The [stack](https://haskellstack.org) help landing page shows how to install
this build tool.

With [ghcup](https://www.haskell.org/ghcup/) we're able to install multiple GHC
versions and multiple cabal versions on a system but at the same time select
which one of each to work with so stack's GHC version set for this project and
the system one brought up by ghcup can be different.

```
% stack exec ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.8.3
% ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.1
```

### Quick Start

* Download the Chromar source.
```
% git clone https://github.com/azardilis/Chromar.git
% cd Chromar
```

* Fire up a REPL and have a go, either [write a model](#writing) there and then
  or [load a model](#loading) module from file.

  Notice that `stack repl` imports all modules whereas `cabal repl` only
  imports module Chromar, the only module properly exposed by the package.
  Build tools have their quirks!

```haskell
% stack repl
*Internal.RuleQuotes Chromar Chromar.Core Chromar.Experiment Chromar.MAttrs
Chromar.MRuleParser Chromar.Multiset Chromar.RExprs Chromar.RuleQuotesE
Internal.RuleQuotes> :set prompt "> "
>
```

```haskell
% cabal repl
*Chromar> :set prompt "> "
>
```

### <a name="writing">Write a Model</a>
After we set the prompt, the sessions in each REPL are verbatim the same with
the following example model.
```haskell
> :set -XQuasiQuotes
> data Agent = A { a :: Double } deriving (Eq, Show)
>
> -- The line that follows is a rule applied to state and time.
> [rule| A{a=x} --> A{a='x+1'} @'x' |] (ms [A{a=1}, A{a=1}, A{a=2}, A{a=3}]) 5.0
[ Rxn {lhs = [(A {a = 1.0},1)], rhs = [(A {a = 2.0},1)], rate = 2.0}
, Rxn {lhs = [(A {a = 2.0},1)], rhs = [(A {a = 3.0},1)], rate = 2.0}
, Rxn {lhs = [(A {a = 3.0},1)], rhs = [(A {a = 4.0},1)], rate = 3.0}
]
```

### <a name="loading">Load a Model</a>
A model saved to file can be loaded into the REPL. The test suites such as
[simple](/models/simple.hs) can be loaded this way or loaded as test suites.

Load a module.

```haskell
% stack repl
> :load models/simple
[ 1 of 10] Compiling Chromar.Multiset
[ 2 of 10] Compiling Chromar.Core
[ 3 of 10] Compiling Internal.RuleQuotes
[ 4 of 10] Compiling Chromar.RExprs
[ 5 of 10] Compiling Chromar.MRuleParser
[ 6 of 10] Compiling Chromar.MAttrs
[ 7 of 10] Compiling Chromar.RuleQuotesE
[ 8 of 10] Compiling Chromar.Experiment
[ 9 of 10] Compiling Chromar
[10 of 10] Compiling Main
Ok, 10 modules loaded.
*Main> :browse Main
data Agent = A {x :: Int}
r1 :: [(Agent, Int)] -> Time -> [Rxn Agent]
r2 :: [(Agent, Int)] -> Time -> [Rxn Agent]
na :: Er Agent Int
nx :: Er Agent Int
s :: Multiset Agent
model :: Model Agent
main :: IO ()
*Main>
*Main> -- run the main function of the Main module
*Main> :main
2 10
3 10
...
18 10
19 10
*Main>
```

Bring up the REPL with the simple test-suite using stack.

```haskell
% stack repl chromar:simple
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/pdejoux/.ghci
[1 of 1] Compiling Main
Ok, one module loaded.
*Main> :browse
data Agent = A {x :: Int}
r1 :: [(Agent, Int)] -> Time -> [Rxn Agent]
r2 :: [(Agent, Int)] -> Time -> [Rxn Agent]
na :: Er Agent Int
nx :: Er Agent Int
s :: Multiset Agent
model :: Model Agent
main :: IO ()
*Main> :main
2 10
2 10
...
19 10
20 10
```

Bring up the REPL with the simple test-suite using cabal.

```haskell
% cabal repl test:simple
Build profile: -w ghc-8.10.1 -O1
In order, the following will be built (use -v for more details):
BuildInplaceOnly
 - chromar-0.1.0.0 (test:simple) (first run)
Preprocessing test suite 'simple' for chromar-0.1.0.0..
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main
Ok, one module loaded.
*Main> :main
2 10
2 10
...
19 10
20 10
```
