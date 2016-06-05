module Output where

import ColouredPetriNets
import Graphics.Gnuplot.Value.Tuple
import Graphics.Gnuplot.Simple as G
import Data.List (intercalate, transpose)


data Observable = ObsInt Int
                | ObsDouble Double

instance Show Observable where
  show (ObsInt n)    = show n
  show (ObsDouble x) = show x

singl :: a -> [a]
singl = (:[])

instance C Observable where
  text (ObsInt n)    = (singl . shows) n
  text (ObsDouble x) = (singl . shows) x


applyObs :: [State a] -> [Multiset a -> Observable] -> [[(Time, Observable)]]
applyObs ss fs = [zip ts (map f xs) | f <- fs] where
  xs = getMs ss
  ts = getTs ss


plotOut :: (Show a) => FilePath -> [State a] -> [Multiset a -> Observable] -> IO ()
plotOut fn ss fs = G.plotLists [PNG fn] obs where
  obs = applyObs ss fs


printOut :: (Show a) => [State a] -> [Multiset a -> Observable] -> IO ()
printOut ss fs = showObs obs where
  obs = applyObs ss fs


getObsOnly :: [(Time, Observable)] -> [Observable]
getObsOnly = map snd


show' :: [(Time, Observable)] -> IO ()
show' tobs = print (show t ++ " " ++ (unwords . map show) obs) where
  obs = getObsOnly tobs
  t = fst (head tobs)


showObs :: [[(Time, Observable)]] -> IO ()
showObs obss = mapM_ show' (transpose obss)
