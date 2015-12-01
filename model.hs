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
  let traj = simulate gen rules init
  printTrajectory $ take n traj
  where rules = [grow, createLeaf, photosynthesis, maintenance]
        init = ms [B 100, L 0 0, L 0 1, R 2]
