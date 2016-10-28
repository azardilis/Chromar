{-# LANGUAGE  QuasiQuotes #-}

import Chromar

--- Agents
data Token = C { pos    :: (Int, Int),
                 x      :: Double }
           | T { ncells :: Int } deriving (Eq, Show)



--- Other definitions
idx :: Token -> Int
idx (C{pos=(i, _), x=_}) = i
idx _ = undefined


isT :: Token -> Bool
isT (T{ncells=_}) = True
isT _             = False


isC :: Token -> Bool
isC (C{pos=_, x=_}) = True
isC _               = False


f = fromIntegral . floor
c = fromIntegral . ceiling


nextTo :: (Int, Int) -> (Int, Int) -> Bool
nextTo (pos, n) (pos', n') = (pos == n') || (n == pos')

mov :: (Int, Int) -> Int -> (Int, Int)
mov (pos, _) n = (pos, n+1)

np :: (Int, Int) -> Int -> (Int, Int)
np (_, nb) n = (n+1, nb)

gr :: Int -> Double
gr n = 10.0 / (fromIntegral n)


--- Rules
df = [rule|
       C{pos=p, x=x}, C{pos=p', x=x'} -->
       C{pos=p, x=x-1}, C{pos=p', x=x'+1} @x [(nextTo p p') && (x>0)]
     |]


growth = [rule|
           T{ncells=n}, C{pos=p, x=x} -->
           T{ncells=n+1}, C{pos=mov p n, x=f (x/2.0)}, C{pos=np p n, x=c (x/2.0)} @(gr n) [True]
         |]


--- Initial state
s0 = ms [ T{ncells=10 },
          C{pos=(1, 2), x=50.0},
          C{pos=(2, 3), x=0.0 },
          C{pos=(3, 4), x=0.0 },
          C{pos=(4, 5), x=0.0 },
          C{pos=(5, 6), x=0.0 },
          C{pos=(6, 7), x=0.0 },
          C{pos=(7, 8), x=0.0 },
          C{pos=(8, 9), x=0.0 },
          C{pos=(9, 10),x=0.0 },
          C{pos=(10, 0),x=0.0 } ]


model :: Model Token
model = Model { rules     = [growth, df],
                initState = s0 }

        
--- Observables
cv :: Multiset Token -> Obs
cv m = sd obss where
  obss = map (\(el, _) -> x el) m


sd :: [Double] -> Obs
sd m = sqrt ((sum [(obs - mean)^^2 | obs <- m]) / n) where
  n       = fromIntegral $ length m
  mean    = (sum m) / n


mean :: [Double] -> Double
mean xs = (sum xs)  / (fromIntegral $ length xs)


nCells = Observable { name = "nCells",
                      gen  = sumM (fromIntegral . ncells) . select isT }

var = Observable { name = "var",
                   gen  = cv . select isC }

conc1 = Observable { name = "conc1",
                     gen  = sumM x . selectAttr idx 1  . select isC }

conc2 = Observable { name = "conc2",
                     gen  = sumM x . selectAttr idx 2 . select isC }

conc3 = Observable { name = "conc3",
                     gen  = sumM x . selectAttr idx 3 . select isC }

totalConc = Observable { name = "total",
                         gen  = sumM x . select isC }


main :: IO ()
main = run model nsteps observables where
  nsteps = 1000
  observables  = [nCells, conc1, var]

