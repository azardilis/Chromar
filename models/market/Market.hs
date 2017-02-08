{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import qualified System.Random as R
import Data.Random.Normal
import Chromar


data Agent
  = Market { sgen :: [Double] }
  | Agent
    { idx :: Int
    , wealth :: Double
    }
  deriving (Eq, Show)


isAgent (Agent {idx=i, wealth=w}) = True
isAgent _ = False


mkPopulation :: R.StdGen -> Multiset Agent
mkPopulation g = ms (market : agents)
  where
    n = 100
    mu = 100.0
    ws = take n $ normals' (mu, mu/5) g
    market = Market { sgen = take 20000 $ R.randomRs (0.0, 1.0) g }
    agents = [Agent {idx=i, wealth=w} | (i, w) <- zip [1..n] ws]


calcGini :: Multiset Agent -> Double
calcGini s = sum [abs (w-w') | w <- ws, w' <- ws] / (2*n*sWealth)
  where
    ws = map wealth (toList s)
    sWealth = sum ws
    n = fromIntegral (length ws)


maxWealth :: Multiset Agent -> Double
maxWealth s = maximum ws
  where
    ws = map wealth (toList s)


gini = Observable { name = "gini", gen  = calcGini . select isAgent }

maxW = Observable { name = "maxW", gen = maxWealth . select isAgent }

totalW = Observable { name = "totalW", gen = sumM wealth . select isAgent }


rsplit :: [Double] -> Double -> ([Double], Double, Double)
rsplit (share:ss) n = (ss, w, w')
  where
    w = share * n
    w' = n - w
rsplit [] _ = error "not enough random numbers!"


wsplit :: [Double] -> Double -> ([Double], Double, Double)
wsplit (share:ss) n =
  if share > 0.5
    then (ss, n, 0.0)
    else (ss, 0.0, n)
wsplit [] _ = error "not enough random numbers!"


tsplit :: [Double] -> Double -> ([Double], Double, Double)
tsplit (share:ss) n = (ss, w + (tax / 2.0), w' + (tax / 2.0))
  where
    tax = 0.35 * n
    afterTax = n - tax
    w = afterTax * share
    w' = afterTax - w
tsplit [] _ = error "not enough random numbers!"


ngen (g, _, _) = g

w1 (_, w, _) = w

w2 (_, _, w) = w


$(return []) ----------


transaction =
  [rule| Market{sgen=g}, Agent{idx=i, wealth=w}, Agent{idx=k, wealth=w'} -->
           Market{sgen=ngen (tsplit g (w+w')) }, Agent{idx=i, wealth=w1 (tsplit g (w+w'))},
             Agent{idx=k, wealth=w2 (tsplit g (w+w')) } @1.0 [i /= k] |]


main :: IO ()
main = do
  g <- R.getStdGen
  let s = mkPopulation g
  let m = Model {rules = [transaction], initState = s}
  runW m 1000 "models/market/out/outGini.txt" [gini, maxW, totalW]
