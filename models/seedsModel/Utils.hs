module Main where

import Control.Monad hiding (when)
import qualified System.Random as R
import Data.Random.Normal
import Chromar.Fluent
import Env

data Attrs = Attrs
    { psi :: !Double
    } deriving (Eq, Show)

data Agent
    = Seed { attr :: !Attrs
           , dg :: !Double
           , art :: !Double
           , mass :: !Double}
    | Plant { attr :: !Attrs
            , dg :: !Double
            , wct :: !Double
            , mass :: !Double
            , area :: !Double}
    | FPlant { attr :: !Attrs
             , dg :: !Double
             , mass :: !Double
             , area :: !Double}
    deriving (Eq, Show)

pchron = 720

marea m pt = m * (slaCot * exp(-slaE * pt))
  where
    slaCot = 2*0.144
    slaE = 0.02

par = when day (constant p) `orElse` (constant 0)
  where
    p = 1.6 / 24.0

rue = 3.0

data DState = DState
    { dtime :: Double
    , mixt :: [Agent]
    } deriving (Show)

data Summ = Summary Int Int Int

showSumm (t, (Summary nseeds nplants nfplants)) =
  show t ++ " "  ++ show nseeds ++ " " ++ show nplants ++ " " ++ show nfplants

isSeed (Seed{ attr=at, dg=d, art=a }) = True
isSeed _ = False

isPlant (Plant { attr=at, dg=d, wct=w}) = True
isPlant _ = False

isFPlant (FPlant { attr=at, dg=d } ) = True
isFPlant _ = False

accNs :: Agent -> (Int, Int, Int) -> (Int, Int, Int)
accNs (Seed{}) (ns, nf, nfp) = (ns+1, nf, nfp)
accNs (Plant{}) (ns, nf, nfp) = (ns, nf+1, nfp)
accNs (FPlant{}) (ns, nf, nfp) = (ns, nf, nfp+1)

summary :: DState -> (Double, Summ)
summary ds = (t, Summary ns nf nfp)
  where
    m = mixt ds
    t = dtime ds
    (ns, nf, nfp) = foldr accNs (0, 0, 0) m

mkSt :: [Double] -> ([Agent], [Double])
mkSt psis =
  ( [ Seed {attr = Attrs {psi = pi}, dg = 0.0, art = 0.0, mass=1.6e-5}
    | pi <- pss
    ]
  , drop n psis)
  where
    n = 10000
    pss = take n psis
     
stepF :: DState -> DState
stepF DState { dtime=t, mixt=m } = DState { dtime=t+1, mixt=concatMap foo m }
  where
    mst = at moist t
    tmp = at temp t
    cpar = at par t
    foo = transAgent . (devAgent (t, mst, tmp, cpar))

devAgent :: (Double, Double, Double, Double) -> Agent -> Agent
devAgent (t, mst, tmp, _) (Seed {attr = atr, dg = d, art = a, mass=m}) =
  Seed {attr = atr, dg = d + (htu t a (psi atr)), art = a + (arUpd mst tmp), mass=m}
devAgent (t, _, _, cpar) (Plant {attr = atr, dg = d, wct = w, mass=m, area=a}) =
  Plant {attr=atr, dg = d + pt * fp (wcUpd t w), wct = wcUpd t w, mass=m', area=a'}
  where
    pt = at ptu t
    m' = m + (a * cpar * rue)
    a' = max a (marea m' d)
devAgent (t, _, _, cpar) (FPlant {attr = atr, dg = d, mass=m, area=a}) =
  FPlant {attr=atr, dg = d + ds, mass=m', area=a'}
  where
    ds = at disp t
    m' = m + (a * cpar * rue)
    a' = max a (marea m' d)

transAgent :: Agent -> [Agent]
transAgent s@Seed{attr=atr, dg=d, art=ar, mass=m}
  | d > 1000.0 = [Plant{attr=atr, dg=0.0, wct=0.0, mass=m, area=marea m 0.0}]
  | otherwise = [s]
transAgent p@Plant{attr=atr, dg=d, wct=w,mass=m, area=a}
  | d > 2604.0 = [FPlant{attr=atr,dg=0.0, mass=m, area=a}]
  | otherwise = [p]
transAgent f@FPlant{attr=atr,dg=d, mass=m, area=a}
  | d > 8448.0 = replicate (floor $ m/1.6e-5) (Seed{attr=atr, dg=0.0, art=0.0, mass=1.6e-5})
  | otherwise = [f]

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

outDD :: [DState] -> IO ()
outDD dss = mapM_ print (map (show . dtime) dss)

fname i = "out/outDW/outD" ++ (show i) ++ ".txt"

doSimulation :: [Double] -> Int -> IO ([Double])
doSimulation psis i = do
    print i
    let (s, psis') = mkSt psis
    let nsteps = (365 * 24 * 1)
    let initS =
            DState
            { dtime = 0
            , mixt = s
            }
    let traj = take nsteps $ iterate stepF initS
    outDD traj
    return psis'

go :: [Double] -> Int -> IO ()
go psis 0 = return ()
go psis n = do
  psis' <- doSimulation psis n
  go psis' (n-1)

main :: IO ()
main = do
  gen <- R.getStdGen
  let psis = normals' (0.0, 6.0) gen
  go psis 1
