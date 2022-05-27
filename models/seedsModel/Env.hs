module Env where

import Chromar.Fluent
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Map.Strict as Map


dataFile = "data/weatherValencia10yrs.csv"

{-# NOINLINE temp' #-}
temp' = unsafePerformIO (readTable dataFile 4)

{-# NOINLINE photo' #-}
photo' = unsafePerformIO (readTable dataFile 2)

{-# NOINLINE day' #-}
day' = unsafePerformIO (readTable dataFile 3)

{-# NOINLINE moist #-}
moist = unsafePerformIO (readTable dataFile 5)


fi = 0.598
--fi = 0.737
fu = 0

psmax = -5
psmin = -1
psu = -50
psl = -350
psSc = 1.0
tbar = 3.0
tbg = 3.0
tbd = 3.0
kt = 0.12
to = 22


tempBase = constant 3.0
day  = day' <>*> constant 0.0


idev = (*)
       <$> constant 0.374
       <*> (photo' <-*> constant 10.0)

idev' = (/)
       <$> idev
       <*> constant 4.0

idev'' = constant 0.626 <+*> idev'


temp = max <$> (temp' <-*> tempBase) <*> pure 0.0
thermal = when day temp `orElse` constant 0.0

pperiod =
  when (photo' <<*> constant 10.0) (constant 0.626) `orElse`
  (when (photo' <<*> constant 14.0) idev'' `orElse` constant 1.0)

ptu = (*) <$> thermal <*> pperiod


tmin = -3.5
tmax = 6.0
wcsat = 960.0

favTemp temp = temp >= tmin && temp <= tmax


wcAcc wc t = wc + exp k * ((t-tmin)**o) * ((tmax-t)**ksi)
  where
    k   = -5.1748
    o   = 2.2256
    ksi = 0.99590


wcUpd t wc =
  if favTemp ctemp
    then wc'
    else wc
  where
    ctemp = at temp t
    wc' = min (wcAcc wc ctemp) wcsat


fp wc =
  if wc < wcsat
    then fp1
    else fp2
  where
    wcRat = wc / wcsat
    fp1 = 1 - fi + (fi - fu) * wcRat
    fp2 = 1 - fu


arUpd moist temp
  | moist <= psmax && moist >= psu = temp - tbar
  | moist < psu && moist > psl = ((psl - moist) / (psl - psu)) * (temp - tbar)
  | moist <= psmax || moist <= psl = 0.0
  | otherwise = 0.0


psB ar psi =
  if psb' > psmin
     then psb'
     else psmin
  where
    arlab = arUpd (-200) 20
    dsat = 40
    psb' = psi - psSc * (ar / (arlab * dsat * 24) )


htuSub ar psi moist temp = (moist - psB ar psi) * (temp - tbg)

htuOpt ar psi moist temp = (moist - mpsB) * (to - tbg)
  where
    mpsB = psB ar psi + kt * (temp - to)

---t : time
--- a : afterripening
--- psi : initial dorm
htu t a psi
  | moistt > psb && tempt > tbg && tempt < to = htuSub ar psi moistt tempt
  | mpsB < moistt && tempt > to = htuOpt a psi moistt tempt
  | otherwise = 0.0                                
  where
    tempt = at temp t
    moistt = at moist t
    ar = a + arUpd moistt tempt
    psb = psB ar psi
    mpsB = psB ar psi + kt * (tempt-to)


disp = when (ntemp <>*> constant 0.0) ntemp `orElse` (constant 0.0) where
  ntemp = temp <-*> constant tbd
  
------

parseLine :: Int -> T.Text -> (Double, Double)
parseLine n ln = (read $ T.unpack time, read $ T.unpack temp) where
  elems = T.splitOn (T.pack ",") ln
  time = T.dropEnd 1 (T.drop 1 $ elems !! 0)
  temp = elems !! n


readTable :: FilePath -> Int -> IO (Fluent Double)
readTable fn n = do
  contents <- TI.readFile fn
  let vals = map (parseLine n) (T.lines contents)
  return $ flookupM (Map.fromList vals)

------
