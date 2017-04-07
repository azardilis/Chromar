{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import Data.List
import qualified System.Random as R
import Data.Random.Normal
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import SChromar
import Env

data Attrs = Attrs
  { ind :: !Int
  , psi :: !Double
  } deriving (Ord, Show)

instance Eq Attrs where
  (==) a a' = (ind a == ind a') && (psi a == psi a')

data Agent = 
  Seed { tob :: !Double
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
  deriving (Ord, Show)

instance Eq Agent where
  (==) a a' = (attr a == attr a')

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

mkSt :: IO (Multiset Agent)
mkSt = do
  let n = 100
  gen <- R.getStdGen
  let psis = take n $ normals' (0.0, 1.0) gen
  return $
    ms
     ( [ Seed {tob=0.0, attr = Attrs {ind = i, psi = pi}, dg = 0.0, art = 0.0}
      | (pi, i) <- zip psis [1 .. n]
      ] )

$(return [])

dev =
  [rule| Seed{attr=atr, dg=d, art=a} -->
             Seed{attr=atr, dg = d + (htu time a (psi atr)), art=a + (arUpd moist temp)} @1.0 |]
  
trans =
  [rule| Seed{tob=tb, attr=atr, dg=d, art=a}-->
             Plant{tob=time,attr=atr, dg=0.0, wct=0.0} @log' d |]

devp =
  [rule| Plant{dg=d, wct=w} --> Plant{dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w} @1.0 |]
  
transp =
  [rule| Plant{tob=tb,attr=a, dg=d, wct=w} -->
                  FPlant{tob=time,attr=a,dg=0.0} @logf' d |]

devfp = [rule| FPlant{dg=d} --> FPlant{dg=d+disp} @1.0 |]

transfp =
    [rule| FPlant{tob=tb,attr=a,dg=d} -->
                  {1} Seed{tob=time, attr=a, dg=0.0, art=0.0} @logs' d |]

fname = "out/outm.txt"


accNs :: Agent -> Int -> (Int, Int, Int) -> (Int, Int, Int)
accNs Seed{} n (ns, np, nfp) = (ns+n, np, nfp)
accNs Plant{} n (ns, np, nfp) = (ns, np+n, nfp)
accNs FPlant{} n (ns, np, nfp) = (ns, np, nfp+n)

nTyps :: MultiSet Agent -> (Int, Int, Int)
nTyps mix = MS.foldOccur accNs (0, 0, 0) mix

showSt :: SimState Agent -> String
showSt sst =
    intercalate
        " "
        [ show (t sst)
        , show ns
        , show np
        , show nfp
        ]
  where
    mix = s sst
    (ns, np, nfp) = nTyps mix

writeTraj :: FilePath -> [SimState Agent] -> IO ()
writeTraj fn ssts = writeFile fn (unlines $ map showSt ssts)

main = do
  si <- mkSt
  g <- R.getStdGen
  let rules = [dev, trans, devp, transp, devfp, transfp]
  let tend = (365 * 1 * 24)
  let traj = takeWhile (\sst -> t sst < tend) (simulate g rules si)
  writeTraj fname traj
