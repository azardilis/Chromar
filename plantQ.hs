{-# LANGUAGE  QuasiQuotes #-}

import qualified System.Random as R
import RuleQuotes
import ColouredPetriNets
import Output


--- Agents
data Token = L { m :: Double } |
             B { c :: Double } |
             R { n :: Int } deriving (Eq, Show)

--- Rules
growth       = [rule| L{m=m}, B{c=c} --> L{m=m+1.0}, B{c=c-1.0} @c/m (c>0) |]
assimilation = [rule| L{m=m}, B{c=c} --> L{m=m}, B{c=c+1} @m (True) |]
leafCreation = [rule| R{n=n}         --> R{n=n+1}, L{m=0} @0.01 (True) |]


--- Initial state
initState = ms [L{m=0.01}, L{m=0.02}, B{c=2}, R{n=2}]


--- observables
rosetteMass :: ObservableGen Token
rosetteMass = ObservableGen { name = "rosetteMass",
                              gen  = calcRosetteMass } where
  calcRosetteMass s = ObsDouble (sum [m * fromIntegral n | (L{m=m}, n) <- s])

carbon :: ObservableGen Token
carbon = ObservableGen { name = "carbon",
                         gen  = getCarbon } where
  getCarbon s = ObsDouble (sum [c | (B{c=c}, _) <- s])

nLeaves :: ObservableGen Token
nLeaves = ObservableGen { name = "nLeaves",
                          gen  = getLeaves } where
  getLeaves s = ObsInt (sum [n | (R{n=n}, _) <- s])
  


--- invariants
countLs :: Multiset Token -> Int
countLs s = sum [n | (L{m=m}, n) <- s]


--- run
main :: IO ()
main = do
  gen <- R.getStdGen
  let n = 2000
  let traj = simulate gen rules initState
  plotOut "plot.png" (take n traj) observables where
    rules = [growth, assimilation, leafCreation]
    observables = [rosetteMass, carbon, nLeaves]

