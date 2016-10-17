{-# LANGUAGE  QuasiQuotes #-}

import ColouredPetriNets
import Ext

data Agent = A { a :: Int, b :: Int}
           | B { c :: Int} deriving (Eq, Show)  


s :: Multiset Agent
s = ms [A{a=1, b=2}, B{c=1}]


r = [rule|A{a=x, b=y}, B{c=x} --> A{a=x, b=y}, B{c=x+1} @1.0 [True]|]


