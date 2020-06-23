{-# LANGUAGE RecordWildCards #-}

module Chromar.Core
    ( Rule
    , Time
    , Rxn(..)
    , State(..)
    , Model(..)
    , getM, getMs, getT, getTs
    , stepRule, stepRxn, fullRate, nrepl
    , simulateRule, simulateRxn
    , printTrajectory, writeTrajectory
    ) where

import Prelude hiding (init)
import qualified System.Random as R (StdGen, randomR)
import Chromar.Multiset (Multiset, diff, plus, mults)

data Rxn a = Rxn
    { lhs :: Multiset a
    , rhs :: Multiset a
    , rate :: !Double
    }
    deriving Eq

instance Show a => Show (Rxn a) where
    showsPrec _ Rxn{..} =
        showString "Rxn {lhs = "
        . shows lhs
        . showString ", rhs = "
        . shows rhs
        . showString ", rate = "
        . shows rate
        . showChar '}'

    showList [] = showString "[]"
    showList (x : xs) =
        showString "[ "
        . shows x
        . foldr
            (\e m -> showString "\n, " . shows e . m)
            (showString "\n]")
            xs

type Rule a = Multiset a -> Time -> [Rxn a]

type Time = Double

data State a =
    State !(Multiset a)
          !Time
          !Int

-- (mixture, time, num of steps)
instance (Show a) => Show (State a) where
    show (State m t n) = show m ++ show t ++ show n

data Model a =
    Model
        { rules :: [Rule a]
        , initState :: Multiset a
        }

getM :: State a -> Multiset a
getM (State m _ _) = m

getMs :: [State a] -> [Multiset a]
getMs = map getM

getT :: State a -> Time
getT (State _ t _) = t

getTs :: [State a] -> [Time]
getTs = map getT

fullRate :: (Eq a) => (Multiset a, Multiset a, Double) -> Double
fullRate (m1, m2, br) = fromIntegral (mults m1 m2) * br

nrepl :: ([Int], [a]) -> [a]
nrepl (mults', elems) = concat [replicate m e | (m, e) <- zip mults' elems]

apply :: (Eq a) => Rxn a -> Multiset a -> Multiset a
apply rxn mix = mix `diff` lhs rxn `plus` rhs rxn

selectRxn :: Double -> Double -> [Rxn a] -> Rxn a
selectRxn _ _ [] = error "deadlock"
selectRxn _ _ [rxn] = rxn
selectRxn acc n (rxn:rxns)
    | n < acc' = rxn
    | otherwise = selectRxn acc' n rxns
    where
        acc' = acc + rate rxn

sample :: R.StdGen -> [Rxn a] -> (Rxn a, Double, R.StdGen)
sample gen rxns =
    (selectRxn 0.0 b rxns, dt, g2)
    where
        totalProp = sum $ map rate rxns
        (a, g1) = R.randomR (0.0, 1.0) gen
        (b, g2) = R.randomR (0.0, totalProp) g1
        dt = log (1.0 / a) / totalProp

stepRxn :: (Eq a) => [Rxn a] -> (R.StdGen, State a) -> (R.StdGen, State a)
stepRxn rxns (gen, State mix t n) =
    (gen', State mix' (t + dt) (n + 1))
    where
        (rxn, dt, gen') = sample gen rxns
        mix' = apply rxn mix

stepRule :: (Eq a) => [Rule a] -> (R.StdGen, State a) -> (R.StdGen, State a)
stepRule rules' (gen, State mix t n) =
    (gen', State mix' (t + dt) (n + 1))
    where
        rxns = concatMap (\r -> r mix t) rules'
        actRxns = filter (\r -> rate r > 0.0) rxns
        (rxn, dt, gen') = sample gen actRxns
        mix' = apply rxn mix

-- | Simulate by iterating 'stepRxn' after first initializing the rules.
simulateRxn :: (Eq a) => R.StdGen -> [Rule a] -> Multiset a -> [State a]
simulateRxn gen rules' init =
    map snd $ iterate (stepRxn rxns) (gen, State init 0.0 0)
    where
        rxns = concatMap (\r -> r init 0.0) rules'

-- | Simulate by iterating 'stepRule'.
simulateRule :: (Eq a) => R.StdGen -> [Rule a] -> Multiset a -> [State a]
simulateRule gen rules' init = map snd $ iterate (stepRule rules') (gen, State init 0.0 0)

printTrajectory :: (Show a) => [State a] -> IO ()
printTrajectory = mapM_ (putStrLn . showState)

writeTrajectory :: (Show a) => FilePath -> [State a] -> IO ()
writeTrajectory fn states = writeFile fn (unlines $ map showState states)

showState :: (Show a) => State a -> String
showState (State m t n) = unwords [show t, show n, show m]
