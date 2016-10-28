module Chromar.Observables where

import qualified System.Random as R
import Chromar.Multiset
import Chromar.Core

type Obs = Double
type ObsF a = Multiset a -> Obs
type TObs = (Time, [Obs])


data Observable a = Observable { name :: String,
                                 gen  :: ObsF a}


names :: [Observable a] -> [String]
names = map name


gens :: [Observable a] -> [ObsF a]
gens = map gen


select :: (a->Bool) -> Multiset a -> Multiset a
select f = filter (\(el, _) -> f el)


aggregate :: (a->Int->Obs->Obs) -> Obs -> Multiset a -> Obs
aggregate f = foldr (\(el, n) s -> f el n s)


sumM :: (a->Obs) -> Multiset a -> Obs
sumM f m = sum (map (\(el, n) -> f el * fromIntegral n) m)


countM :: Multiset a -> Obs
countM s = sum [fromIntegral n | (el, n) <- s]


selectAttr :: (Eq b) => (a->b) -> b -> Multiset a -> Multiset a
selectAttr f v = filter (\(el, _) -> f el == v)


applyObs :: [State a] -> [ObsF a] -> [TObs]
applyObs ss fs = [(t, map ($ s) fs) | (State s t _) <- ss]


printObs :: (Show a) => [State a] -> [Observable a] -> IO ()
printObs ss fs = do
  putStrLn header
  showObs obs where
    obs = applyObs ss (gens fs)
    obsNames = names fs
    header = unwords ("time" : obsNames)


show' :: TObs -> IO ()
show' tobs = putStrLn $ showTObs tobs


showObs :: [TObs] -> IO ()
showObs = mapM_  show'


showTObs :: TObs -> String
showTObs (t, obss) = show t ++ " " ++ obssS where
  obssS = unwords (map show obss)


writeObs :: (Show a) => FilePath -> [State a] -> [Observable a] -> IO ()
writeObs fn ss fs = writeFile fn (unlines obsS) where
    obs      = applyObs ss (gens fs)
    obsNames = names fs
    header   = unwords ("time" : obsNames)
    obsS     = header : map showTObs obs


run :: (Eq a, Show a) => Model a -> Int -> [Observable a] -> IO ()
run (Model {rules=rs, initState=s})  n obss = do
  rgen <- R.getStdGen
  let traj = take n (simulate rgen rs s)
  printObs traj obss


runW :: (Eq a, Show a) => Model a -> Int -> FilePath -> [Observable a] -> IO ()
runW (Model {rules=rs, initState=s})  n fn obss = do
  rgen <- R.getStdGen
  let traj = take n (simulate rgen rs s)
  writeObs fn traj obss
