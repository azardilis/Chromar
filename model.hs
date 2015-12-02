import ColouredPetriNets
import System.Environment (getArgs)
import qualified System.Random as R


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
gmax = 0.4

rt :: Double
rt = 1.0971

p1 :: Double
p1 = 0.005

p2 :: Double
p2 = 0.016

p :: Double
p = 0.05

photoRate :: Double
photoRate = 5.7

d :: Int -> Int -> Double
d i nl
  | (nl-i) >= 4 = 0.2782*(fromIntegral $ nl-i) + 0.1211
  | otherwise = -0.0572*(fromIntegral $ nl-i) + 1.2472

m0 :: Double
m0 = 0.0

angle :: Int -> Double
angle ni = -0.0559*(fromIntegral ni)+1.1043

area :: Double -> Double
area m = kma * m where
  kma = 0.05 - (0.05 / exp(95*m))

effArea :: Int -> Double -> Int -> Double
effArea n m i = cos (angle (n-i)) * area m

leafInit :: Int -> Double
leafInit n 
    |n > 13  = 0.0618
    |otherwise = 0.0286

-- TODO: use quasi-quotes to make the definition of rules simpler

-- L m i, B c -> L (m+1) i, B (c-1)
grow :: Rule Token
grow mix = [ rxn m i c k n nl l | (L m i, k) <- mix
                                , (B c, n) <- mix,
                                  (R nl, l) <- mix]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Int -> Int -> Rxn Token
        rxn m i c k n nl l=
                   Rxn { lhs = ms [L m i, B c]
                       , rhs = ms [L (m+1) i, B (c-1)]
                       , rate = gmax * d i nl * c *
                                fromIntegral k * fromIntegral n}

-- R age -> R (age+1), L m0 age
createLeaf :: Rule Token
createLeaf mix = [ rxn age n | (R age, n) <- mix ]
  where rxn :: Int -> Int -> Rxn Token
        rxn age n = Rxn { lhs = ms [R age]
                        , rhs = ms [R (age+1), L m0 age]
                        , rate = leafInit n}

-- L m i, B c -> L m i, B (c+f(m,i))
photosynthesis :: Rule Token
photosynthesis mix = [ rxn m i c k n nl l| (L m i, k) <- mix,
                                           (B c, n) <- mix,
                                           (R nl, l) <- mix]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Int -> Int -> Rxn Token
        rxn m i c k n nl l =
          Rxn { lhs = ms [L m i, B c, R nl]
              , rhs = ms [L m i, B (c+dc), R nl]
              , rate = photoRate * fromIntegral k * fromIntegral n *fromIntegral l}
          where dc = effArea nl m i

-- L m i, B c -> L m i, B (c-g(m))
maintenance :: Rule Token
maintenance mix = [ rxn m i c k n nl l| (L m i, k) <- mix
                                  , (B c, n) <- mix
                                  , (R nl, l) <- mix]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Int -> Int -> Rxn Token
        rxn m i c k n nl l =
          Rxn { lhs = ms [L m i, B c, R nl]
              , rhs = ms [L m i, B (c-dc), R nl]
              , rate =  rm * fromIntegral k * fromIntegral n * fromIntegral l}
          where dc = 1.0 -- should this be a function of m?
                rm = 24*rt*(p1*c/ (fromIntegral nl) + p2 * effArea nl m i)


translocation :: Rule Token
translocation mix = [ rxn m i c k n nl l| (L m i, k) <- mix
                                        , (B c, n) <- mix
                                        , (R nl, l) <- mix]
  where rxn :: Double -> Int -> Double -> Int -> Int -> Int -> Int -> Rxn Token
        rxn m i c k n nl l =
          Rxn { lhs = ms [L m i, B c, R nl]
              , rhs = ms [L (m-dc) i, B (c+dc), R nl]
              , rate =  tR * fromIntegral k * fromIntegral n * fromIntegral l}
          where dc = 1.0 -- should this be a function of m?
                tR = p*effArea nl m i - (c / fromIntegral nl)

-- TODO: add the other rules

main :: IO ()
main = do
  gen <- R.getStdGen
  args <- getArgs
  let n = (read $ head args) :: Int
  let traj = simulate gen rules init
  printTrajectory $ take n traj
  where rules = [grow, createLeaf, photosynthesis, maintenance]
        init = ms [B 100, L 0 0, L 0 1, R 2]
