-- | Convenience functions for running models.
module Chromar.Experiment where

import Chromar.Core
import Chromar.RExprs (Er, at)
import qualified System.Random as R

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

run
    :: (Eq a, ToSpaceSep b)
    => Model a -> Int -> Er a b -> IO ()
run Model{rules = rs, initState = s} n er = do
    rgen <- R.getStdGen
    let traj = map (applyEr er) $ take n (simulate rgen rs s)
    mapM_ (putStrLn . toSpaceSep) traj

writeRows :: (ToSpaceSep a, ToSpaceSep b) => FilePath -> a -> [b] -> IO ()
writeRows fn nms traj = do
    let header = toSpaceSep nms
    let rows = header : map toSpaceSep traj
    writeFile fn (unlines rows)

runW
    :: (Eq a, ToSpaceSep b)
    => Model a -> Int -> FilePath -> [String] -> Er a b -> IO ()
runW Model{rules = rs, initState = s} n fn nms er = do
    rgen <- R.getStdGen
    let traj = map (applyEr er) $ take n (simulate rgen rs s)
    writeRows fn nms traj

runT
    :: (Eq a, ToSpaceSep b)
    => Model a -> Time -> Er a b -> IO ()
runT Model{rules = rs, initState = s} t er = do
    rgen <- R.getStdGen
    let traj = map (applyEr er) $ takeWhile (\s' -> getT s' < t) (simulate rgen rs s)
    mapM_ (putStrLn . toSpaceSep) traj

runTW
    :: (Eq a, ToSpaceSep b)
    => Model a -> Time -> FilePath -> [String] -> Er a b -> IO ()
runTW Model{rules = rs, initState = s} t fn nms er = do
    rgen <- R.getStdGen
    let traj = map (applyEr er) $ takeWhile (\s' -> getT s' < t) (simulate rgen rs s)
    writeRows fn nms traj

-- $setup
-- >>> :set -XScopedTypeVariables -XPackageImports
-- >>> import "template-haskell" Language.Haskell.TH
-- >>> import "QuickCheck" Test.QuickCheck
