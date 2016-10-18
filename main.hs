{-# LANGUAGE  QuasiQuotes #-}

import ColouredPetriNets
import Ext

data Agent = A { a :: Int }
           | B { b :: Int } deriving (Eq, Show)  


state :: Multiset Agent
state = ms [A{a=1}, A{a=2}, B{b=2}]


r = [rule|A{a=x}, A{a=x+1} --> A{a=x}, A{a=x+1} @1.0 [True] |]


