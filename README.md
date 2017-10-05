## Chromar language and simulator

### Quick start
* Install the [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) tool
* Download and install the Chromar library
```
% git clone https://github.com/azardilis/Chromar.git
% cd Chromar
% stack install
```
* Investigate things in the repl (to use concrete syntax rule you'll need to turn on the QuasiQuotes Haskell extension)
```
% stack repl
ghci> :set -XQuasiQuotes
ghci> data Agent = A { x :: Int } deriving (Eq, Show)
ghci> let state = ms [A{x=1}, A{x=1}, A{x=2}, A{x=3}]
ghci> let r = [rule| A{x=x} --> A{x='x+1'} @'x' |]
ghci> let t = 5.0
ghci> r state t -- get all concrete reactions from rule r at time t at current state
```
or
* Write a model to a file and then load in the repl (see example models in the /models directory)
```
% stack repl
ghci> :l Model
ghci> let nsteps = 1000
ghci> run model nsteps observables
```
