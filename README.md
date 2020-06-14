## Chromar language and simulator

### Quick start
* Install the [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) tool
* Download and install the Chromar library
```
% git clone https://github.com/azardilis/Chromar.git
% cd Chromar
% stack build
```
* We can investigate things in the repl if we enable the quasiquotes language
  extension. The ghci session will be cleaner if we supress a couple of warnings.
```
% stack repl
> :set -XQuasiQuotes -fno-warn-name-shadowing -fno-warn-unused-matches
> data Agent = A {x :: Double} deriving (Eq, Show)
> let state = ms [A{x=1}, A{x=1}, A{x=2}, A{x=3}]
> let r = [rule| A{x=x} --> A{x='x+1'} @'x' |]
> let t = 5.0
> r state t
[Rxn {lhs = [(A {x = 1.0},1)], rhs = [(A {x = 2.0},1)], rate = 2.0},Rxn {lhs = [(A {x = 2.0},1)], rhs = [(A {x = 3.0},1)], rate = 2.0},Rxn {lhs = [(A {x = 3.0},1)], rhs = [(A {x = 4.0},1)], rate = 3.0}]
```
or
* Write a model to a file and then load in the repl (see example models in the /models directory)
```
% stack repl
ghci> :l Model
ghci> let nsteps = 1000
ghci> run model nsteps observables
```
