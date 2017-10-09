module Chromar.Experiment where

{-
Convenience functions for running models
-}

import           Chromar.Core
import           Chromar.Multiset
import           Chromar.RExprs
import           Data.List
import qualified System.Random    as R

class ToSpaceSep a  where
    toSpaceSep :: a -> String

instance ToSpaceSep Int where
    toSpaceSep i = show i

instance ToSpaceSep Double where
    toSpaceSep d = show d

instance (Show a1, Show a2) =>
         ToSpaceSep (a1, a2) where
    toSpaceSep (v1, v2) = show v1 ++ " " ++ show v2

instance (Show a1, Show a2, Show a3) =>
         ToSpaceSep (a1, a2, a3) where
    toSpaceSep (v1, v2, v3) = show v1 ++ " " ++ show v2 ++ " " ++ show v3

instance (Show a1, Show a2, Show a3, Show a4) =>
         ToSpaceSep (a1, a2, a3, a4) where
    toSpaceSep (v1, v2, v3, v4) =
        show v1 ++ " " ++ show v2 ++ " " ++ show v3 ++ " " ++ show v3

applyEr :: Er a b -> State a -> b
applyEr er (State m t n) = at er m t

run
    :: (Eq a, ToSpaceSep b)
    => Model a -> Int -> Er a b -> IO ()
run (Model {rules = rs
           ,initState = s}) n er = do
    rgen <- R.getStdGen
    let traj = map (applyEr er) $ take n (simulate rgen rs s)
    mapM_ (putStrLn . toSpaceSep) traj

runW
    :: (Eq a, ToSpaceSep b)
    => Model a -> Int -> FilePath -> Er a b -> IO ()
runW (Model {rules = rs
            ,initState = s}) n fn obss = undefined

runT
    :: (Eq a, ToSpaceSep b)
    => Model a -> Time -> Er a b -> IO ()
runT (Model {rules = rs
            ,initState = s}) t obss = undefined

runTW
    :: (Eq a, ToSpaceSep b)
    => Model a -> Time -> FilePath -> Er a b -> IO ()
runTW (Model {rules = rs
             ,initState = s}) t fn obss = undefined
