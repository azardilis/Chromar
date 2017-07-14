
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import Graphics.Rendering.Chart.Easy hiding (at)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)
import Data.List (intercalate)
import Data.Random.Normal
import qualified System.Random as R
import SChromar
import Env

data Attrs = Attrs
    { psi :: !Double
    } deriving (Eq, Ord, Show)

data Agent
    = Seed { mass :: !Double
           , attr :: !Attrs
           , dg :: !Double
           , art :: !Double}
    | Plant { mass :: !Double
            , area :: !Double
            , attr :: !Attrs
            , dg :: !Double
            , wct :: !Double}
    | FPlant { mass :: !Double
             , area :: !Double
             , attr :: !Attrs
             , dg :: !Double}
    deriving (Eq, Ord, Show)

marea m d = m * sla
  where
    slaCot = 0.144
    slaE = 0.002
    sla = slaCot * exp(-slaE * d)

rue temp = max (-1.8 + 0.07 * temp) 0.0

log' :: Double -> Double
log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

logf' :: Double -> Double
logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))

logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))

mkSt :: IO (Multiset Agent)
mkSt = do
  let n = 10
  gen <- R.getStdGen
  let psis = take n $ normals' (0.0, 1.0) gen
  return $
    ms
     ( [ Plant {mass=1.6e-5, area=marea 1.6e-5 0.0, attr = Attrs {psi = pi}, dg = 0.0, wct = 0.0}
      | pi<- psis
      ] )

$(return [])

dev =
    [rule| Seed{attr=atr, dg=d, art=a} -->
           Seed{attr=atr, dg = d + (htu time a (psi atr)), art=a + (arUpd moist temp)}
           @1.0
   |]
  
trans =
    [rule|
        Seed{mass=m, attr=atr, dg=d, art=a} -->
        Plant{mass=m, area=2e-6, attr=atr, dg=0.0, wct=0.0}
        @log' d
  |]

devp =
    [rule|
         Plant{dg=d, wct=w, mass=m, area=a} -->
         Plant{dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w, mass=m+(marea m d * par * (rue temp')), area=max (marea (m+marea m d * par * (rue temp')) d) a}
         @1.0
   |]

transp =
    [rule|
        Plant{mass=m, area=a, attr=atr, dg=d, wct=w} -->
        FPlant{mass=m, area=a, attr=atr, dg=0.0}
        @logf' d

|]

devfp =
    [rule|
        FPlant{dg=d, mass=m, area=a} -->
        FPlant{dg=d+disp, mass=m+(marea m d * par * (rue temp')), area=max (marea (m+marea m d * par * (rue temp')) d) a}
        @1.0
   |]

transfp =
    [rule|
         FPlant{mass=m, area=a, attr=atr, dg=d} -->
         {ceiling (m/1.6e-5/1000.0)} Seed{mass=1.6e-5, attr=atr, dg=0.0, art=0.0}
         @logs' d
   |]

----------------------running stuff
fname = "models/seedsModel/out/outFreqs.txt"

accNs :: Agent -> Int -> (Int, Int, Int) -> (Int, Int, Int)
accNs Seed{} n (ns, np, nfp) = (ns+n, np, nfp)
accNs Plant{} n (ns, np, nfp) = (ns, np+n, nfp)
accNs FPlant{} n (ns, np, nfp) = (ns, np, nfp+n)

nTyps :: MultiSet Agent -> (Int, Int, Int)
nTyps mix = MS.foldOccur accNs (0, 0, 0) mix

writeNs :: FilePath -> [SimState Agent] -> IO ()
writeNs fn stss = writeFile fn (unlines $ map foo stss)
  where
    foo st =
        let (ns, nf, nfp) = nTyps (s st)
        in let tm = t st
           in show tm ++ " " ++ show ns ++ " " ++ show nf ++ " " ++ show nfp

getSumm :: SimState Agent -> (Double, (Int, Int, Int))
getSumm sst = (tm, (ns, np, nfp))
  where
    tm = t sst
    mix = s sst
    (ns, np, nfp) = nTyps mix

nTypss :: [SimState Agent] -> ([(Double, Int)], [(Double, Int)], [(Double, Int)])
nTypss ssts = (nss, nps, nfps)
  where
    nss = [(t, ns) | sst <- ssts, let (t, (ns, _, _)) = getSumm sst]
    nps = [(t, np) | sst <- ssts, let (t, (_, np, _)) = getSumm sst]
    nfps = [(t, nfp) | sst <- ssts, let (t, (_, _, nfp)) = getSumm sst]

showStN :: SimState Agent -> String
showStN sst =
    intercalate
        " "
        [ show ns
        , show np
        , show nfp
        ]
  where
    mix = s sst
    (ns, np, nfp) = nTyps mix

devSum :: SimState Agent -> String
devSum sst = show $ MS.foldOccur (\ag n s -> s+ (dg ag)) 0.0 mix
  where
    mix = s sst

mSum :: SimState Agent -> String
mSum sst = show (t sst) ++ " " ++ (show $ MS.foldOccur (\ag n s -> s+ (mass ag)) 0.0 mix)
  where
    mix = s sst

writeTraj :: FilePath -> [SimState Agent] -> IO ()
writeTraj fn ss = writeFile fn (unlines $ map mSum ss)

showTraj :: [SimState Agent] -> IO ()
showTraj ss =
    mapM_
        print
        (map (\s -> mSum s ++ " " ++ showStN s ++ " " ++ (show $ devSum s)) ss)

massCalc :: SimState Agent -> (Double, Double)
massCalc sst = (tm, MS.foldOccur (\ag n s -> s+ (mass ag)) 0.0 mix)
  where
    tm = (t sst) / 24.0
    mix = s sst

massF :: [SimState Agent] -> [(Double, Double)]
massF = map massCalc

devCalc :: SimState Agent -> (Double, Double)
devCalc sst = (tm, MS.foldOccur (\ag n s -> s+ (dg ag)) 0.0 mix)
  where
    tm = (t sst) / 24.0
    mix = s sst

devF = map devCalc

everyN n xs =
  case drop (n - 1) xs of
    [] -> []
    (y:ys) -> y : everyN n ys

go :: IO ([SimState Agent])
go = do
  sin <- mkSt
  g <- R.getStdGen
  let rules = [dev, trans, devp, transp, devfp, transfp]
  let tend = 100
  return $ takeWhile (\sst -> (t sst) < tend) $ simulate g rules sin

main = do
    sin <- mkSt
    g <- R.getStdGen
    let rules = [dev, trans, devp, transp, devfp, transfp]
    let tend = (365*2*24)
    let traj = takeWhile (\sst -> (t sst) < tend) $ simulate g rules sin
    writeNs fname traj
