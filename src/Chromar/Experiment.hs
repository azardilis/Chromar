-- | Convenience functions for running models.
module Chromar.Experiment
    ( ToSpaceSep(..)
    , run, runT, runW, runTW
    ) where

import Chromar.Core (Model(..), State(..), Time, simulateRule, getT)
import Chromar.Enriched.Syntax (Er, at)
import System.Random (StdGen, getStdGen)

class ToSpaceSep a where
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
        show v1 ++ " " ++ show v2 ++ " " ++ show v3 ++ " " ++ show v4

instance (Show a1, Show a2, Show a3, Show a4, Show a5) =>
         ToSpaceSep (a1, a2, a3, a4, a5) where
    toSpaceSep (v1, v2, v3, v4, v5) =
        show v1 ++
        " " ++ show v2 ++ " " ++ show v3 ++ " " ++ show v4 ++ " " ++ show v5

instance (Show a1, Show a2, Show a3, Show a4, Show a5, Show a6) =>
         ToSpaceSep (a1, a2, a3, a4, a5, a6) where
    toSpaceSep (v1, v2, v3, v4, v5, v6) =
        show v1 ++
        " " ++
        show v2 ++
        " " ++ show v3 ++ " " ++ show v4 ++ " " ++ show v5 ++ " " ++ show v6

instance (Show a) => ToSpaceSep [a] where
    toSpaceSep xs = unwords $ map show xs

applyEr :: Er a b -> State a -> b
applyEr er (State m t _n) = at er m t

writeRows :: (ToSpaceSep a, ToSpaceSep b) => FilePath -> a -> [b] -> IO ()
writeRows fn nms traj = do
    let header = toSpaceSep nms
    let rows = header : map toSpaceSep traj
    writeFile fn (unlines rows)

simN :: Eq a => Int -> StdGen -> Model a -> [State a]
simN n rgen model = take n $ simulateRule rgen model

simPred :: Eq a => (State a -> Bool) -> StdGen -> Model a -> [State a]
simPred p rgen model = takeWhile p $ simulateRule rgen model

run
    :: (Eq a, ToSpaceSep b)
    => Model a -> Int -> Er a b -> IO ()
run model n er = do
    rgen <- getStdGen
    let traj = applyEr er <$> simN n rgen model
    mapM_ (putStrLn . toSpaceSep) traj

runW
    :: (Eq a, ToSpaceSep b)
    => Model a -> Int -> FilePath -> [String] -> Er a b -> IO ()
runW model n fn nms er = do
    rgen <- getStdGen
    let traj = applyEr er <$> simN n rgen model
    writeRows fn nms traj

runT
    :: (Eq a, ToSpaceSep b)
    => Model a -> Time -> Er a b -> IO ()
runT model t er = do
    rgen <- getStdGen
    let traj = applyEr er <$> simPred (\s' -> getT s' < t) rgen model
    mapM_ (putStrLn . toSpaceSep) traj

runTW
    :: (Eq a, ToSpaceSep b)
    => Model a -> Time -> FilePath -> [String] -> Er a b -> IO ()
runTW model t fn nms er = do
    rgen <- getStdGen
    let traj = applyEr er <$> simPred (\s' -> getT s' < t) rgen model
    writeRows fn nms traj

-- $setup
-- >>> :set -XScopedTypeVariables -XPackageImports
-- >>> import "template-haskell" Language.Haskell.TH
-- >>> import "QuickCheck" Test.QuickCheck
