{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

import ColouredPetriNets
import Ext
import Fluent


data Agent = A { a :: Int }
           | B { b :: Int } deriving (Eq, Show)  


--- Fluents
day = between 6 18 (constant True) (constant False)

light = repeatEvery 24 day

temp = when light (constant 21.0) `orElse` constant 19.0


state :: Multiset Agent
state = ms [A{a=1}, A{a=2}, B{b=2}]

$(return [])

r = [rule|A{a=x}, A{a=x+1} --> A{a=x}, A{a=x+1} @1.0 [light] |]


