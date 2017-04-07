module Chromar.Sim where

import Data.Map.Lazy (Map)
import Data.Map.Lazy((!))
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified System.Random as R
import Data.List (find)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Chromar.Multiset
import Chromar.IMultiset
import Chromar.RuleQuotes

type Rid = Int

type Time = Double

type Rule a = Multiset a -> Time -> [Rxn a]

data SimState a = SimState
    { t :: Double
    , s :: MultiSet a
    , rcount :: Int
    , incl :: Map a (Set Rid)
    , rxns :: Map Rid (Rxn a)
    }

data State a =
    State !(Multiset a)
          !Time
          !Int

data Model a = Model
    { rules :: [Rule a]
    , initState :: Multiset a
    }

data Rxn a = Rxn
    { lhs :: MultiSet a
    , rhs :: MultiSet a
    , rate :: !Double
    , act :: !Double  
    } deriving (Eq, Show)

getM :: State a -> Multiset a
getM (State m _ _) = m

getMs :: [State a] -> [Multiset a]
getMs = map getM

getT :: State a -> Time
getT (State _ t _) = t

getTs :: [State a] -> [Time]
getTs = map getT

getRxns
  :: (Ord a)
  => SimState a -> [Rxn a]
getRxns = undefined
-- instead of getting the reactions and then using selectRxn on the resulting list
--- we can do instead direclty on the rxnsMap using
--- mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)

initialise
    :: (Ord a)
    => [Rule a] -> Multiset a -> SimState a
initialise rs m =
    SimState
    { t = 0.0
    , s = MS.fromOccurList m
    , rcount = rc
    , incl = inclMap
    , rxns = M.fromList rxnMap
    }
  where
    rxs = concat [ r m 0.0 | r <- rs ]
    rc = length rxs
    rxnMap = zip [1 .. rc] rxs
    inclMap =
        M.fromListWith
            (S.union)
            [ (a, S.singleton i)
            | (i, r) <- rxnMap
            , (a, _) <- MS.toOccurList (lhs r) ]

delRxns
    :: Ord a
    => a -> Map a (Set Rid) -> Map Rid (Rxn a) -> MultiSet a -> Map Rid (Rxn a)
delRxns ag incl rxs s' =
    M.fromList
        [ ( ri
          , Rxn
            { lhs = lhs rx
            , rhs = rhs rx
            , rate = rate rx
            , act = nact
            })
        | ri <- S.toList (incl ! ag)
        , let rx = rxs ! ri
        , let nact = fullRate (lhs rx, s', rate rx),
          nact == 0.0 ]

updRxns
    :: Ord a
    => a -> Map a (Set Rid) -> Map Rid (Rxn a) -> MultiSet a -> Map Rid (Rxn a)
updRxns ag incl rxs s' =
    M.fromList
        [ ( ri
          , Rxn
            { lhs = lhs rx
            , rhs = rhs rx
            , rate = rate rx
            , act = nact
            })
        | ri <- S.toList (incl ! ag)
        , let rx = rxs ! ri
        , let nact = fullRate (lhs rx, s', rate rx),
          nact > 0.0 ]

negUpdate
    :: Ord a
    => Rxn a -> SimState a -> SimState a
negUpdate rxn SimState {t = t
                       ,s = s
                       ,rcount = rcount
                       ,incl = incl
                       ,rxns = rxns} =
    SimState
    { t = t
    , s = s'
    , rcount = rcount
    , incl = incl'
    , rxns = rxns'
    }
  where
    s' = s `mdiff` (lhs rxn)
    l = fst . head $ (MS.toOccurList $ lhs rxn)
    dRxns = delRxns l incl rxns s'
    uRxns = updRxns l incl rxns s'
    dRxnIds = S.fromList (M.keys dRxns)
    uRxnIds = S.fromList (M.keys uRxns)
    lRs = S.union uRxnIds (S.difference (incl ! l) dRxnIds)
    rxns' = M.difference (M.union uRxns rxns) dRxns
    incl' = if (null lRs) then (M.delete l incl)
            else  (M.insert l lRs incl)

posUpdate
    :: Ord a
    => Rxn a -> [Rule a] -> SimState a -> SimState a
posUpdate rxn rs SimState {t = t
                          ,s = s
                          ,rcount = rcount
                          ,incl = incl
                          ,rxns = rxns}
    | MS.member r s =
        SimState
        { t = t
        , s = s'
        , rcount = rcount
        , incl = incl'
        , rxns = rxns'
        }
    | otherwise =
        SimState
        { t = t
        , s = s'
        , rcount = rcount'
        , incl = incl''
        , rxns = rxns''
        }
  where
    s' = s `mplus` (rhs rxn)
    r = fst . head $ (MS.toOccurList $ rhs rxn)
    uRxns = updRxns r incl rxns s'
    uRxnIds = S.fromList (M.keys uRxns)
    lRs = S.union uRxnIds (incl ! r)
    rxns' = M.union uRxns rxns
    incl' = if (null lRs) then (M.delete r incl)
            else  (M.insert r lRs incl)
    rrxns =
        concat
            [ rl (ms [r]) t
            | rl <- rs ]
    nrxns = length rrxns
    rcount' = rcount + nrxns
    rxns'' = M.union (M.fromList (zip [rcount + 1 .. rcount'] rrxns)) rxns
    incl'' = M.insert r (S.fromList [rcount + 1 .. rcount']) incl
    
updateState
    :: (Ord a)
    => Rxn a -> [Rule a] -> Time -> SimState a -> SimState a
updateState rxn rs dt SimState {t = t
                               ,s = s
                               ,rcount = rcount
                               ,incl = incl
                               ,rxns = rxns} = simSt''
  where
    t' = t + dt
    simSt' =
        negUpdate
            rxn
            (SimState
             { t = t'
             , s = s
             , rcount = rcount
             , incl = incl
             , rxns = rxns
             })
    simSt'' = posUpdate rxn rs simSt'
  
fullRate
    :: (Ord a)
    => (MultiSet a, MultiSet a, Double) -> Double
fullRate (m1, m2, br) = fromIntegral (mmults m1 m2) * br

nrepl :: ([Int], [a]) -> [a]
nrepl (mults, elems) = concat [replicate m e | (m, e) <- zip mults elems]

apply
    :: (Ord a)
    => Rxn a -> MultiSet a -> MultiSet a
apply rxn mix = mix `mdiff` (lhs rxn) `mplus` (rhs rxn)

selectRxn :: Double -> Double -> [Rxn a] -> Rxn a
selectRxn _ _ [] = error "deadlock"
selectRxn _ _ [rxn] = rxn
selectRxn acc n (rxn:rxns)
    | n < acc' = rxn
    | otherwise = selectRxn acc' n rxns
  where
    acc' = acc + (act rxn)

sample :: R.StdGen -> [Rxn a] -> (Rxn a, Double, R.StdGen)
sample gen rxns = (selectRxn 0.0 b rxns, dt, g2)
  where
    totalProp = sum $ map act rxns
    (a, g1) = R.randomR (0.0, 1.0) gen
    (b, g2) = R.randomR (0.0, totalProp) g1
    dt = log (1.0 / a) / totalProp

step
    :: (Ord a)
    => [Rule a] -> (R.StdGen, SimState a) -> (R.StdGen, SimState a)
step rules (gen, sm) = (gen', sm')
  where
    actRxns = filter (\r -> act r > 0.0) (M.elems (rxns sm))
    (rxn, dt, gen') = sample gen actRxns
    sm' = updateState rxn rules dt sm

simulate
    :: (Ord a)
    => R.StdGen -> [Rule a] -> Multiset a -> [SimState a]
simulate gen rules init =
    map snd $ iterate (step rules) (gen, initSmState)
  where
    initSmState = initialise rules init
