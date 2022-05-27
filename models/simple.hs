{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

import Chromar

-- Agent declarations
{-# ANN type Agent "HLint: ignore Use newtype instead of data" #-}
data Agent = A { x :: Int } deriving (Eq, Show)

$(return [])

-- Rules
r1, r2 :: [(Agent, Int)] -> Time -> [Rxn Agent]
r1 = [rule| A{x=x}, A{x=y} --> A{x='x+1'}, A{x='y-1'} @'1.0' ['y > 0'] |]
r2 = [rule| A{x=x} --> A{x='x'}, A{x='0'} @'1.0' |]

na, nx :: Er Agent Int
na = [er| select A{x=x}; aggregate (count . 'count + 1') '0' |]
nx = [er| select A{x=x}; aggregate (count . 'count + x') '0' |]

s :: Multiset Agent
s = ms [A{x=5}, A{x=5}]

model :: Model Agent
model = Model{ rules = [r1, r2], initState = s }

main :: IO ()
main = let nsteps = 100 in run model nsteps (erZip na nx)
