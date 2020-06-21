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
  extension.
```haskell
% stack repl
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
