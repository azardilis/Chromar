## Chromar Language and Simulator

### Build Tools

We show how to get going with both stack and cabal. Pick either one or both.

Stack will install a compiler version and pick package versions of dependencies
that known to work together. These are published on stackage.org. We've picked
a resolver in [`./stack.yaml`](/stack.yaml).

The [stack](https://haskellstack.org) help landing page shows how to install
this build tool.

With [ghcup](https://www.haskell.org/ghcup/) we're able to install multiple GHC
versions and multiple cabal versions on a system but at the same time select
which one of each to work with so stack's GHC version set for this project and
the selected one brought up by ghcup can be different.

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

* Fire up a REPL and have a go.

  Notice that `stack repl` imports all the modules whereas `cabal repl` only
  imports the module Chromar. This is the only module exposed by the package.

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

  After we set the prompt, the sessions in each REPL are verbatim the same.
```haskell
> :set -XQuasiQuotes
> data Agent = A { a :: Double } deriving (Eq, Show)
>
> -- The line that follows is a rule applied to state and time.
> [rule| A{a=x} --> A{a='x+1'} @'x' |] (ms [A{a=1}, A{a=1}, A{a=2}, A{a=3}]) 5.0
[ Rxn {lhs = [(A {a = 1.0},1)], rhs = [(A {a = 2.0},1)], rate = 2.0}
, Rxn {lhs = [(A {a = 2.0},1)], rhs = [(A {a = 3.0},1)], rate = 2.0}
, Rxn {lhs = [(A {a = 3.0},1)], rhs = [(A {a = 4.0},1)], rate = 3.0}
```

or
* Write a model to a file and then load in the repl (see example models in the /models directory)

``` haskell
% stack repl
ghci> :l Model
ghci> let nsteps = 1000
ghci> run model nsteps observables
```
