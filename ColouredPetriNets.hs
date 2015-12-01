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
          putStrLn $ unwords [show t, show n, show m]


-- Model

data Token = L Double Int -- mass and index
           | B Double -- carbon
           | R Int -- age
  deriving (Eq, Show, Ord)

index :: Token -> Int
index (L m i) = i
index _ = error "token is not of type L"

mass :: Token -> Double
mass (L m i) = m
mass _ = error "token is not of type L"

carbon :: Token -> Double
carbon (B c) = c
carbon _ = error "token is not of type B"

-- Constants
gmax :: Double
gmax = 1.0

d :: Int -> Double
d n = fromIntegral (n+1)

m0 :: Double
m0 = 0.0

-- TODO: use quasi-quotes to make the definition of rules simpler

-- L m i, B c -> L (m+1) i, B (c-1)
grow :: Rule Token
grow mix = [ rxn m i c k n | (L m i, k) <- mix
                           , (B c, n) <- mix ]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Rxn Token
        rxn m i c k n =
          Rxn { lhs = ms [L m i, B c]
              , rhs = ms [L (m+1) i, B (c-1)]
              , rate = gmax * d(i) * c *
                       fromIntegral k * fromIntegral n }

-- R age -> R (age+1), L m0 age
createLeaf :: Rule Token
createLeaf mix = [ rxn age n | (R age, n) <- mix ]
  where rxn :: Int -> Int -> Rxn Token
        rxn age n = Rxn { lhs = ms [R age]
                        , rhs = ms [R (age+1), L m0 age]
                        , rate = fromIntegral n }

-- L m i, B c -> L m i, B (c+f(m,i))
photosynthesis :: Rule Token
photosynthesis mix = [ rxn m i c k n | (L m i, k) <- mix
                                     , (B c, n) <- mix ]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Rxn Token
        rxn m i c k n =
          Rxn { lhs = ms [L m i, B c]
              , rhs = ms [L m i, B (c+dc)]
              , rate = fromIntegral k * fromIntegral n }
          where dc = m -- FIXME: add term for the effective area

-- L m i, B c -> L m i, B (c-g(m))
maintenance :: Rule Token
maintenance mix = [ rxn m i c k n | (L m i, k) <- mix
                                  , (B c, n) <- mix ]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Rxn Token
        rxn m i c k n =
          Rxn { lhs = ms [L m i, B c]
              , rhs = ms [L m i, B (c-dc)]
              , rate = fromIntegral k * fromIntegral n }
          where dc = 1.0 -- should this be a function of m?

-- TODO: add the other rules

main :: IO ()
main = do
  gen <- R.getStdGen
  args <- getArgs
  let n = (read $ head args) :: Int
  let init = ms [B 100, L 0 0, L 0 1, R 2]
  let traj = simulate gen rules init
  printTrajectory $ take n traj
  where rules = [grow, createLeaf, photosynthesis, maintenance]
