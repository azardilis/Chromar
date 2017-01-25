module Main where

import qualified System.Random as R
import Data.Random.Normal
import Chromar.Fluent
import Env

data Attrs = Attrs
  { ind :: Int
  , psi :: Double
  } deriving (Eq, Show)


data Agent
  = Seed { attr :: Attrs
        ,  dg :: Double
        ,  art :: Double}
  | Plant { attr :: Attrs
         ,  dg :: Double
         ,  wct :: Double}
  | FPlant { attr :: Attrs
          ,  dg :: Double}
  deriving (Eq, Show)


data DState = DState { dtime :: Double,
                       mixt :: [Agent] } deriving (Show)

data Summ = Summary Int Int Int

showSumm (t, (Summary nseeds nplants nfplants)) =
  show t ++ " "  ++ show nseeds ++ " " ++ show nplants ++ " " ++ show nfplants

isSeed (Seed{ attr=at, dg=d, art=a }) = True
isSeed _ = False

isPlant (Plant { attr=at, dg=d, wct=w}) = True
isPlant _ = False

isFPlant (FPlant { attr=at, dg=d } ) = True
isFPlant _ = False

log' :: Double -> Double
log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

logf' :: Double -> Double
logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))

logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))

summary :: DState -> (Double, Summ)
summary ds = (t, Summary nseeds nplants nfplants)
  where
    m = mixt ds
    t = dtime ds
    nseeds = length $ filter isSeed m
    nplants = length $ filter isPlant m
    nfplants = length $ filter isFPlant m

mkSt :: IO [Agent]
mkSt = do
  let n = 1000
  gen <- R.getStdGen
  let psis = take n $ normals' (0.0, 1.0) gen
  return $
     ( [ Seed {attr = Attrs {ind = i, psi = pi}, dg = 0.0, art = 0.0}
      | (pi, i) <- zip psis [1 .. n]
      ] )
     
mkSt' :: [Agent]
mkSt' = ( replicate 3 (Seed{attr = Attrs {ind=0, psi=(-0.5)}, dg=0.0, art=0.0}) ++
          replicate 18 (Seed{attr = Attrs {ind=0, psi=(-0.388)}, dg=0.0, art=0.0}) ++
          replicate 66 (Seed{attr = Attrs {ind=0, psi=(-0.2777)}, dg=0.0, art=0.0}) ++
          replicate 161 (Seed{attr = Attrs {ind=0, psi=(-0.1666)}, dg=0.0, art=0.0}) ++
          replicate 252 (Seed{attr = Attrs {ind=0, psi=(-0.05555)}, dg=0.0, art=0.0}) ++
          replicate 3 (Seed{attr = Attrs {ind=0, psi=0.5}, dg=0.0, art=0.0}) ++
          replicate 18 (Seed{attr = Attrs {ind=0, psi=0.388}, dg=0.0, art=0.0}) ++
          replicate 66 (Seed{attr = Attrs {ind=0, psi=0.2777}, dg=0.0, art=0.0}) ++
          replicate 161 (Seed{attr = Attrs {ind=0, psi=0.1666}, dg=0.0, art=0.0}) ++
          replicate 252 (Seed{attr = Attrs {ind=0, psi=0.05555}, dg=0.0, art=0.0}) )


stepF :: DState -> DState
stepF DState { dtime=t, mixt=m } = DState { dtime=t+1, mixt=fmap foo m }
  where
    foo = transAgent . (devAgent t)

devAgent :: Time -> Agent -> Agent
devAgent t (Seed {attr = atr, dg = d, art = a}) =
  Seed {attr = atr, dg = d + (htu t a (psi atr)), art = a + (arUpd mst tmp)}
  where
    mst = at moist t
    tmp = at temp t
devAgent t (Plant {attr = atr, dg = d, wct = w}) =
  Plant {attr=atr, dg = d + pt * fp (wcUpd t w), wct = wcUpd t w}
  where
    pt = at ptu t
devAgent t (FPlant {attr = atr, dg = d}) = FPlant {attr=atr, dg = d + ds}
  where
    ds = at disp t

transAgent :: Agent -> Agent
transAgent Seed{attr=atr, dg=d, art=a}
  | d > 1000.0 = Plant{attr=atr, dg=0.0, wct=0.0}
  | otherwise = Seed{attr=atr, dg=d, art=a}
transAgent Plant{attr=a, dg=d, wct=w}
  | d > 2604.0 = FPlant{attr=a,dg=0.0}
  | otherwise = Plant{attr=a, dg=d, wct=w}
transAgent FPlant{attr=a,dg=d}
  | d > 8448.0 = Seed{attr=a, dg=0.0, art=0.0}
  | otherwise = FPlant{attr=a,dg=d}                   

out :: [DState] -> IO ()
out dss = mapM_ print summs
  where
    summs = map (showSumm . summary) dss

outF :: FilePath -> [DState] -> IO ()
outF fp dss = writeFile fp (unlines liness)
  where
    header = "time nseeds nplants nfplants"
    summs = map (showSumm . summary) dss
    liness = header : summs

summD :: DState -> String
summD ds = show dgs
  where
    m = mixt ds
    dgs = fmap dg m

outD :: [DState] -> IO ()
outD dss = mapM_ print summs
  where
    summs = map summD dss

main :: IO ()
main = do
  s <- mkSt
  let nsteps = (365*24*10)
  let initS = DState { dtime = 0, mixt=s }
  let traj = take nsteps $ iterate stepF initS
  outF "outD.txt" traj

