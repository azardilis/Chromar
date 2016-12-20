{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import qualified System.Random as R
import Data.Random.Normal
import Chromar
import Env


data Attrs = Attrs
  { ind :: Int
  , psi :: Double
  } deriving (Eq, Show)


data Agent
  = Seed { attr :: Attrs
        ,  dg :: Double
        ,  art :: Double }
  | Plant { attr :: Attrs
         ,  dg :: Double
         ,  wct :: Double}
  | FPlant { attr :: Attrs
          ,  dg :: Double}
  deriving (Eq, Show)

isSeed (Seed{ attr=at, dg=d, art=a }) = True
isSeed _ = False

isPlant (Plant { attr=at, dg=d, wct=w}) = True
isPlant _ = False

isFPlant (FPlant { attr=at, dg=d } ) = True
isFPlant _ = False


log' :: Double -> Double
log' t = 1.0 / (1.0 + exp (-0.1 * (t - 950.0)))

logf' :: Double -> Double
logf' t = 1.0 / (1.0 + exp (-0.1 * (t - 2500.0)))

logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-0.1 * (t - 8000.0)))



nseeds = Observable {name = "nseeds", gen = countM . select isSeed}

nplants = Observable { name = "nplants", gen  = countM . select isPlant }

nfplants = Observable { name = "nfplants", gen = countM . select isFPlant }
           
sdev = Observable { name = "sdev", gen  = sumM dg }


mkSt :: IO (Multiset Agent)
mkSt = do
  let n = 100
  gen <- R.getStdGen
  let psis = take n $ normals' (0.0, 1.0) gen
  return $
    ms
      [ Seed {attr = Attrs {ind = i, psi = pi}, dg = 0.0, art = 0.0}
      | (pi, i) <- zip psis [1 .. n]
      ]

$(return [])

dev =
  [rule| Seed{attr=atr, dg=d, art=a} -->
             Seed{attr=atr, dg = d + (htu time a (psi atr)), art=a + (arUpd moist temp)} @1.0 |]

trans =
  [rule| Seed{attr=atr, dg=d, art=a} --> Plant{attr=atr, dg=0.0, wct=0.0} @log' d |]

devp =
  [rule| Plant{attr=a, dg=d, wct=w} --> Plant{attr=a, dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w} @1.0 |]

transp = [rule| Plant{attr=a, dg=d, wct=w} --> FPlant{attr=a,dg=0.0} @logf' d |]

devfp = [rule| FPlant{attr=a,dg=d} --> FPlant{attr=a,dg=d+disp} @1.0 |]

transfp = [rule| FPlant{attr=a,dg=d} --> Seed{attr=a, dg=0.0, art=0.0} @logs' d |]


main :: IO ()
main = do
  s <- mkSt
  let m =
        Model
        {rules = [dev, trans, devp, transp, devfp, transfp], initState = s}
  runTW m (365 * 10 * 24) "models/seedsModel/out/outV.txt" [nseeds, nplants, nfplants]
