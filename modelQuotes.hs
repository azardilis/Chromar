{-# LANGUAGE  QuasiQuotes #-}

import qualified System.Random as R
import RuleQuotes
import ColouredPetriNets


--- Model definition

-- Agent declarations
data Token = A { x :: Int }
           | B { y :: Int } deriving (Eq, Show)

-- Rule declarations
r = [rule| A{x=x}, B{y=y} --> A{x=x+1}, B{y=y-1} @1.0 (True) |]


initState :: Multiset Token
initState = ms [A{x=1}, A{x=2}, B{y=1}]


--- Running the model
main :: IO ()
main = do
  gen <- R.getStdGen
  let n = 10
  let traj = simulate gen [r] initState
  printTrajectory $ take n traj
