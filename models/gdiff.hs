{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Chromar

--- Agents
data Agent = C
    { cid    :: Int
    , nextTo :: Int
    , x      :: Double
    } deriving (Eq, Show)

f = fromIntegral . floor
c = fromIntegral . ceiling

g :: Double -> Double
g s = gmax * (s / (ks + s))
  where
    gmax = 0.25
    ks = 2.5

between t0 t1 t = t >= t0 && t <= t1

sf = [er| repeatEvery '10.0' (when 'between 0.0 5.0 $time$' 'smax' else '0.0') |]
  where
    smax = 10.0

conc1 = [er| select C{cid=i, nextTo=k, x=x};
             aggregate (conc1 . 'if i==1 then conc1 + x
                                 else 0.0')
                       '0' |]

total = [er| select C{cid=i, nextTo=k, x=x}; aggregate (conc . 'conc + x') '0' |]

n = [er| select C{cid=i, nextTo=k, x=x}; aggregate (count . 'count + 1') '0' |]

mean = [er| '$total$ / $n$' |]

sd = [er| select C{cid=i, nextTo=k, x=x};
          aggregate (acc . 'acc + (x - $mean$)^^2') '0' |]

var = [er| '$sd$ / $n$' |]

$(return [])

--- Rules
df = [rule|
       C{nextTo=p, x=x}, C{cid=p, x=y}  -->
       C{nextTo='p', x='x-1'}, C{cid='p', x='y+1'}  @'x' ['x>0']
     |]

df' = [rule|
       C{cid=p, x=y}, C{nextTo=p, x=x}   -->
       C{cid='p', x='y-1'}, C{nextTo='p', x='x+1'} @'y' ['y>0']
     |]

growth =
    [rule|
           C{nextTo=p, x=x} -->
           C{nextTo='round $n$+1', x='f (x/2.0)'}, C{cid='round $n$+1', nextTo='p', x='c (x/2.0)'}
           @'g $sf$'
         |]

--- Initial state
s0 = ms [ C{cid=1, nextTo=2, x=50.0},
          C{cid=2, nextTo=3, x=0.0},
          C{cid=3, nextTo=4, x=0.0},
          C{cid=4, nextTo=5, x=0.0},
          C{cid=5, nextTo=6, x=0.0},
          C{cid=6, nextTo=7, x=0.0},
          C{cid=7, nextTo=8, x=0.0},
          C{cid=8, nextTo=9, x=0.0},
          C{cid=9, nextTo=10, x=0.0},
          C{cid=10, nextTo=0, x=0.0} ]

model :: Model Agent
model = Model { rules     = [df, df', growth],
                initState = s0 }

main :: IO ()
main =
    runTW
        model
        20.0
        "out/out.txt"
        ["t", "n", "var", "conc1"]
        (zipEr4 time n var conc1)
