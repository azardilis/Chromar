{-# LANGUAGE  QuasiQuotes #-}

import Ext
import Defs

data Agent = A { a :: Int, b :: Int}
           | B { c :: Int} deriving (Show)  

r = [rule|A{a=x, b=y}, B{c=x}|]


