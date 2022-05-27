{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import Data.List
import qualified System.Random as R
import GHC.Exts (groupWith, the)
import Data.Random.Normal
import Chromar
import Env

data Attrs = Attrs
  { ind :: !Int
  , psi :: !Double
  } deriving (Eq, Show)


data Agent
  = System { germTimes :: ![Double]
           , flrTimes :: ![Double]
           , ssTimes :: ![Double]}
  | Seed { tob :: !Double
         , attr :: !Attrs
         , dg :: !Double
         , art :: !Double}
  | Plant { tob :: !Double
          , attr :: !Attrs
          , dg :: !Double
          , wct :: !Double}
  | FPlant { tob :: !Double
           , attr :: !Attrs
           , dg :: !Double}
  deriving (Eq, Show)



getInd :: Agent -> Int
getInd Seed { attr = a } = ind a
getInd Plant { attr = a } = ind a
getInd FPlant { attr = a } = ind a


isSeed (Seed{ attr=at, dg=d, art=a }) = True
isSeed _ = False

isPlant (Plant { attr=at, dg=d, wct=w}) = True
isPlant _ = False

isFPlant (FPlant { attr=at, dg=d } ) = True
isFPlant _ = False


isSystem (System { germTimes = g }) = True
isSystem _ = False


avg l  = let (t,n) = foldl' (\(b,c) a -> (a+b,c+1)) (0,0) l 
         in (realToFrac(t)/realToFrac(n))

log' :: Double -> Double
--log' t = 1.0 / (1.0 + exp (-0.1 * (t - 950.0)))
log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

logf' :: Double -> Double
-- logf' t = 1.0 / (1.0 + exp (-0.1 * (t - 2500.0)))
logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))


logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))


nseeds = Observable {name = "nseeds", gen = countM . select isSeed}

nplants = Observable { name = "nplants", gen  = countM . select isPlant }

nfplants = Observable { name = "nfplants", gen = countM . select isFPlant }
           
avgGermTime =
  Observable
  { name = "avgGermTime"
  , gen = sumM (avg . germTimes) . select isSystem
  }

avgFlrTime =
  Observable
  { name = "avgFlrTime"
  , gen = sumM (avg . flrTimes) . select isSystem
  }

avgSTime =
  Observable
  { name = "avgSTime"
  , gen = sumM (avg . ssTimes) . select isSystem
  }



dev1 = Observable { name = "dev1", gen = sumM dg . selectAttr getInd 1 }
dev2 = Observable { name = "dev2", gen = sumM dg . selectAttr getInd 2 }
dev3 = Observable { name = "dev3", gen = sumM dg . selectAttr getInd 3 }
dev4 = Observable { name = "dev4", gen = sumM dg . selectAttr getInd 4 }


mkSt :: IO (Multiset Agent)
mkSt = do
  let n = 100
  gen <- R.getStdGen
  let psis = take n $ normals' (0.0, 1.0) gen
  return $
    ms
     ( [ Seed {tob=0.0, attr = Attrs {ind = i, psi = pi}, dg = 0.0, art = 0.0}
      | (pi, i) <- zip psis [1 .. n]
      ] ++ [System{germTimes=[], flrTimes=[], ssTimes=[]}] )


mkSt' :: Multiset Agent
mkSt' =
  ms $
  replicate
    100
    ( Seed {tob = 0.0, attr = Attrs {ind = 1, psi = 0.0}, dg = 0.0, art = 0.0} ) ++
     [System {germTimes = [], flrTimes = [], ssTimes = []}]


mkSt'' :: [Double] -> (Multiset Agent, [Double])
mkSt'' psis =
  ( ms ([ Seed {attr = Attrs {ind = i, psi = pi}, tob=0.0, dg = 0.0, art = 0.0}
    | (pi, i) <- zip pss [1 .. n]
    ] ++ [System{germTimes=[], flrTimes=[], ssTimes=[]}] )
  , drop n psis)
  where
    n = 100
    pss = take n psis


$(return [])


dev =
  [rule| Seed{attr=atr, dg=d, art=a} -->
             Seed{attr=atr, dg = d + (htu time a (psi atr)), art=a + (arUpd moist temp)} @1.0 |]
  
trans =
  [rule| Seed{tob=tb, attr=atr, dg=d, art=a}, System{germTimes=gt} -->
             Plant{tob=time,attr=atr, dg=0.0, wct=0.0}, System{germTimes=(time-tb):gt} @log' d |]

devp =
  [rule| Plant{dg=d, wct=w} --> Plant{dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w} @1.0 |]
  
transp =
  [rule| Plant{tob=tb,attr=a, dg=d, wct=w}, System{flrTimes=ft} -->
                  FPlant{tob=time,attr=a,dg=0.0}, System{flrTimes=(time-tb):ft} @logf' d |]

devfp = [rule| FPlant{dg=d} --> FPlant{dg=d+disp} @1.0 |]

transfp = [rule| FPlant{tob=tb,attr=a,dg=d}, System{ssTimes=st} -->
                   Seed{tob=time, attr=a, dg=0.0, art=0.0}, System{ssTimes=(time-tb):st} @logs' d |]

fname i = "models/seedsModel/out/outS/outS" ++ (show i) ++ ".txt"

doSimulation :: [Double] -> Int -> IO ([Double])
doSimulation psis i = do
  print i
  let (s, psis') = mkSt'' psis
  let t = (365*24*2)
  let m =
         Model
         {rules = [dev, trans, devp, transp, devfp, transfp], initState = s}
  runTW' m t (fname i) nseeds
  return psis'

go :: [Double] -> Int -> IO ()
go psis 0 = return ()
go psis n = do
   psis' <- doSimulation psis n
   go psis' (n-1)

main :: IO ()
main = do
  gen <- R.getStdGen
  let psis = normals' (0.0, 1.0) gen
  go psis 50



-----------------------
showRow (t, val) = show t ++ " " ++ show val

writeTraj fn traj = writeFile fn (unlines rows)
  where
    header = "time" ++ " " ++ "val"
    rows = header : (map showRow traj)

calcDay = floor . (/24)

everyN n xs =
  case drop (n - 1) xs of
    [] -> []
    (y:ys) -> y : everyN n ys

              
runTW'
  :: (Eq a, Show a)
  => Model a -> Time -> FilePath -> Observable a -> IO ()
runTW' (Model {rules = rs
              ,initState = s}) tEnd fn obs = do
  rgen <- R.getStdGen
  let traj = everyN 100 (takeWhile (\s -> getT s < tEnd) $ simulate rgen rs s)
  let ttraj = [ (t, gen obs m) | State m t n <- traj]
  writeTraj fn ttraj


