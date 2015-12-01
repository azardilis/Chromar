module ColouredPetriNets where  

import qualified Data.Map as Map
import qualified System.Random as R
import Data.Maybe (fromMaybe, fromJust)
import Data.List (find, findIndex)
import System.Environment (getArgs)


type Multiset a = [(a, Int)]
data Rxn a = Rxn { lhs :: Multiset a,
                   rhs :: Multiset a,
                   rate :: Double }
  deriving (Eq, Show)
type Rule a = Multiset a -> [Rxn a]
type State a = (Multiset a, Double, Int) -- (mixture, time, num of steps)

frequencies :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
frequencies acc [] = acc
frequencies acc (x:xs) = frequencies (update acc) xs
  where update ((y,n):ys) | x == y = (y,n+1):ys
                          | otherwise = (y,n):update ys
        update [] = [(x,1)]

ms :: (Eq a) => [a] -> Multiset a
ms = frequencies []

diff :: (Eq a) => Multiset a -> Multiset a -> Multiset a
diff [] ys = []
diff ((x,n):xs) ys = sub $ find (\(y,_) -> x == y) ys
  where sub (Just (y,m)) | n-m > 0 = (x,n-m):(diff xs ys)
                         | otherwise = diff xs ys
        sub Nothing = (x,n):(diff xs ys)

plus :: (Eq a) => Multiset a -> Multiset a -> Multiset a
plus [] ys = ys
plus ((x,n):xs) ys = add $ findAndRemove [] (\(y,_) -> x == y) ys
  where add (Just (y,m), ys') = (x,n+m):(plus xs ys')
        add (Nothing, _) = (x,n):(plus xs ys)

findAndRemove :: [a] -> (a -> Bool) -> [a] -> (Maybe a, [a])
findAndRemove acc _ [] = (Nothing, reverse acc)
findAndRemove acc p (x:xs) | p x = (Just x, (reverse acc) ++ xs)
                           | otherwise = findAndRemove (x:acc) p xs

apply :: (Eq a) => Rxn a -> Multiset a -> Multiset a
apply rxn mix = mix `diff` (lhs rxn) `plus` (rhs rxn)

selectRxn :: Double -> Double -> [Rxn a] -> Rxn a
selectRxn _ _ [] = error "deadlock"
selectRxn _ _ [rxn] = rxn
selectRxn acc n (rxn:rxns) | n < acc' = rxn
                           | otherwise = selectRxn acc' n rxns
  where acc' = acc + (rate rxn)

sample :: R.StdGen -> [Rxn a] -> (Rxn a, Double, R.StdGen)
sample gen rxns = (selectRxn 0.0 b rxns,  dt, g2)
  where totalProp = sum $ map rate rxns
        (a, g1) = R.randomR (0.0, 1.0) gen
        (b, g2) = R.randomR (0.0, totalProp) g1
        dt = log (1.0/a) / totalProp

step :: (Eq a) => [Rule a] -> (R.StdGen, State a) -> (R.StdGen, State a)
step rules (gen, (mix, t, n)) = (gen', (mix', t+dt, n+1))
  where rxns = concatMap (\r -> r mix) rules
        (rxn, dt, gen') = sample gen rxns
        mix' = apply rxn mix

simulate :: (Eq a) => R.StdGen -> [Rule a] -> Multiset a -> [State a]
simulate gen rules init =
  map snd $ iterate (step rules) (gen, (init, 0.0, 0))

printTrajectory :: (Show a) => [State a] -> IO ()
printTrajectory states = mapM_ printMixture states
  where printMixture :: (Show a) => State a -> IO ()
        printMixture (m,t,n) =
          putStrLn $ showState (m, t, n)

writeTrajectory :: (Show a) => FilePath -> [State a] -> IO ()
writeTrajectory fn states = 
        let strStates = map showState states
        in writeFile fn (unlines strStates)

showState :: (Show a) => State a -> String
showState (m, t, n) = unwords [show t, show n, show m]
