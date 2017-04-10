{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)
import qualified System.Random as R
import SChromar
import Env

data Agent
    = Plant { mass :: !Double
            , area :: !Double
            , wct :: !Double
            , dg :: !Double}
    deriving (Ord, Show)


instance Eq Agent where
  (==) a a' = (mass a == mass a') && (dg a == dg a')


si = ms [Plant{mass=3.2e-5, area=sla 3.2e-5 0.0 0.0, wct=0.0, dg=0.0}]

pchron = 720

sla m a pt = max a (m*a')
  where
    slaCot = 2*0.144
    slaE = 0.02
    a' = slaCot * exp(-slaE * pt)

par = when day (constant 100) `orElse` (constant 0)

$(return [])

devp =
  [rule| Plant{dg=d, wct=w} --> Plant{dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w} @1.0 |]

devm =
  [rule| Plant{mass=m, area=a, dg=d} --> Plant{mass=m+(sla m a d * par), area=sla m a d}
         @(temp / pchron) |]

fname = "out/outMass.txt"

accNs :: Agent -> Int -> (Double, Double, Double) -> (Double, Double, Double)
accNs Plant{mass=m, area=a, dg=d} n (ar, mass, dg) = (ar+a, mass + m, dg+d)

massArea :: MultiSet Agent -> (Double, Double, Double)
massArea mix = MS.foldOccur accNs (0.0, 0.0, 0.0) mix

showSt :: SimState Agent -> String
showSt sst = show (t sst) ++ " " ++ show  area ++ " " ++ show mass ++ " " ++ show dg
  where
    mix = s sst
    (area, mass, dg) = massArea mix
  
writeTraj :: FilePath -> [SimState Agent] -> IO ()
writeTraj fn ss = writeFile fn (unlines $ map showSt ss)

main = do
  g <- R.getStdGen
  let rules = [devp, devm]
  let tend = 800
  let traj = takeWhile (\sst -> t sst < tend) (simulate g rules si)
  writeTraj fname traj
      


