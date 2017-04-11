{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)
import qualified System.Random as R
import SChromar
import Env

data Agent = FPlant
    { mass :: !Double
    , area :: !Double
    , dg :: !Double
    } deriving (Ord, Show)

instance Eq Agent where
  (==) a a' = (mass a == mass a') && (dg a == dg a')

si = ms [FPlant{mass=3.2e-1, area=sla 3.2e-1 0.0 0.0, dg=0.0}]

pchron = 720

sla m a pt = max a (m*a')
  where
    slaCot = 2*0.144
    slaE = 0.02
    a' = slaCot * exp(-slaE * pt)

par = when day (constant p) `orElse` (constant 0)
  where
    p = 1.6 / 24.0

rue = 3.0

$(return [])


devfp = [rule| FPlant{dg=d} --> FPlant{dg=d+disp} @1.0 |]

devm =
  [rule| FPlant{mass=m, area=a, dg=d} --> FPlant{mass=m+(sla m a d * par * rue), area=sla m a d}
         @(temp / pchron) |]

fname = "out/outMass.txt"

accNs :: Agent -> Int -> (Double, Double, Double) -> (Double, Double, Double)
accNs FPlant{mass=m, area=a, dg=d} n (ar, mass, dg) = (ar+a, mass + m, dg+d)

massArea :: MultiSet Agent -> (Double, Double, Double)
massArea mix = MS.foldOccur accNs (0.0, 0.0, 0.0) mix

showSt :: SimState Agent -> String
showSt sst = show (t sst) ++ " " ++ show  area ++ " " ++ show mass ++ " " ++ show dg
  where
    mix = s sst
    (area, mass, dg) = massArea mix
  
writeTraj :: FilePath -> [SimState Agent] -> IO ()
writeTraj fn ss = writeFile fn (unlines $ map showSt ss)

showTraj :: [SimState Agent] -> IO ()
showTraj ss = mapM_ print (map showSt ss)

main = do
  g <- R.getStdGen
  let rules = [devfp, devm]
  let tend = 500
  let traj = takeWhile (\sst -> t sst < tend) (simulate g rules si)
  writeTraj fname traj
