{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

import Chromar

--- Agents
data Agent = L { m :: Double }
           | B { c :: Double }
           | R { n :: Int }
           deriving (Eq, Show)

$(return [])

--- Rules
growth, assimilation, leafCreation :: [(Agent, Int)] -> Time -> [Rxn Agent]
growth       = [rule| L{m=m}, B{c=c} --> L{m='m+1.0'}, B{c='c-1.0'} @'c/m' ['c-1>0'] |]
assimilation = [rule| L{m=m}, B{c=c} --> L{m='m'}, B{c='c+1'} @'m' ['True'] |]
leafCreation = [rule| R{n=n}         --> R{n='n+1'}, L{m='0'} @'0.0001' ['True'] |]


--- Initial state
s :: Multiset Agent
s = ms [L{m=0.01}, L{m=0.02}, B{c=0.3}, R{n=2}]

--- Observables
rosMass, carbon :: Er Agent Double
rosMass = [er| select L{m=m}; aggregate (tMass. 'tMass + m') '0.0'|]
carbon = [er| select B{c=c}; aggregate (tCarb. 'tCarb + c') '0.0'|]

nLeaves :: Er Agent Integer
nLeaves = [er| select L{m=m}; aggregate (nL. 'nL + 1') '0'|]

model :: Model Agent
model =
    Model
    { rules = [growth, assimilation, leafCreation]
    , initState = s
    }

--- run
main :: IO ()
main = run model nsteps observables
  where
    nsteps = 1000
    observables = erZip3 rosMass carbon nLeaves
