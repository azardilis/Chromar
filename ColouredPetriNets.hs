{-# LANGUAGE BangPatterns #-}

module ColouredPetriNets where  

import qualified System.Random as R
import Data.List (find)


type Multiset a = [(a, Int)]

data Rxn a = Rxn { lhs :: Multiset a,
                   rhs :: Multiset a,
                   rate :: Double }
  deriving (Eq, Show)
           
type Rule a = Multiset a -> [Rxn a]

type Time = Double

data State a = State (Multiset a) !Time !Int -- (mixture, time, num of steps)

instance (Show a) => Show (State a) where
  show (State m t n) = show m ++ show t ++ show n

getM :: State a -> Multiset a
getM (State m _ _) = m

getMs :: [State a] -> [Multiset a]
getMs = map getM

getT :: State a -> Time
getT (State _ t _) = t

getTs :: [State a] -> [Time]
getTs = map getT


frequencies :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
frequencies acc [] = acc
frequencies acc (x:xs) = frequencies (update acc) xs
  where update ((y,n):ys) | x == y = (y,n+1):ys
                          | otherwise = (y,n):update ys
        update [] = [(x,1)]


ms :: (Eq a) => [a] -> Multiset a
ms = frequencies []


occur :: (Eq a) => a -> Multiset a -> Int
occur el m = case lookup el m of
  (Just n) -> n
  Nothing  -> 0


perms :: Int -> Int -> Int
perms n k
  | k > n = 0
  | otherwise = product (take k $ numsToOne n) where
    numsToOne n = enumFromThenTo n (n-1) 1


mults :: (Eq a) => Multiset a -> Multiset a -> Int
mults m1 m2 = product [perms (occur el m2) m | (el, m) <- m1]


fullRate :: (Eq a) => (Multiset a, Multiset a, Double) -> Double
fullRate (m1, m2, br) = fromIntegral (mults m1 m2) * br


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
step rules (gen, State mix t n) = (gen', State mix' (t+dt) (n+1))
  where rxns = concatMap (\r -> r mix) rules
        actRxns = filter (\r -> rate r > 0.0) rxns
        (rxn, dt, gen') = sample gen actRxns
        mix' = apply rxn mix


simulate :: (Eq a) => R.StdGen -> [Rule a] -> Multiset a -> [State a]
simulate gen rules init =
  map snd $ iterate (step rules) (gen, State init 0.0 0)


printTrajectory :: (Show a) => [State a] -> IO ()
printTrajectory states = mapM_ (putStrLn . showState) states


writeTrajectory :: (Show a) => FilePath -> [State a] -> IO ()
writeTrajectory fn states = 
  writeFile fn (unlines $ map showState states)


showState :: (Show a) => State a -> String
showState (State m t n) = unwords [show t, show n, show m]

