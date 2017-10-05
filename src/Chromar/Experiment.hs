applyObs :: [State a] -> [ObsF a] -> [TObs]
applyObs ss fs =
    [ (t, map ($ s) fs)
    | (State s t _) <- ss ]

formatFloatN numOfDecimals floatNum =
    showFFloat (Just numOfDecimals) floatNum ""

printObs
    :: (Show a)
    => [State a] -> [Observable a] -> IO ()
printObs ss fs = do
    putStrLn header
    showObs obs
  where
    obs = applyObs ss (gens fs)
    obsNames = names fs
    header = unwords ("time" : obsNames)

show' :: TObs -> IO ()
show' tobs = putStrLn $ showTObs tobs

showObs :: [TObs] -> IO ()
showObs = mapM_ show'

showTObs :: TObs -> String
showTObs (t, obss) = show t ++ " " ++ obssS
  where
    obssS = unwords (map show obss)

writeObs
    :: (Show a)
    => FilePath -> [Observable a] -> [State a] -> IO ()
writeObs fn fs ss = writeFile fn (unlines obsS)
  where
    obs = applyObs ss (gens fs)
    obsNames = names fs
    header = unwords ("time" : obsNames)
    obsS = header : map showTObs obs

run
    :: (Eq a, Show a)
    => Model a -> Int -> [Observable a] -> IO ()
run (Model {rules = rs
           ,initState = s}) n obss = do
    rgen <- R.getStdGen
    let traj = take n (simulate rgen rs s)
    printObs traj obss

runW
    :: (Eq a, Show a)
    => Model a -> Int -> FilePath -> [Observable a] -> IO ()
runW (Model {rules = rs
            ,initState = s}) n fn obss = do
    rgen <- R.getStdGen
    let traj = take n (simulate rgen rs s)
    writeObs fn obss traj

runT
    :: (Eq a, Show a)
    => Model a -> Time -> [Observable a] -> IO ()
runT (Model {rules = rs
            ,initState = s}) t obss = do
    rgen <- R.getStdGen
    let traj = takeWhile (\s -> getT s < t) (simulate rgen rs s)
    printObs traj obss

runTW
    :: (Eq a, Show a)
    => Model a -> Time -> FilePath -> [Observable a] -> IO ()
runTW (Model {rules = rs
             ,initState = s}) t fn obss = do
    rgen <- R.getStdGen
    let traj = takeWhile (\s -> getT s < t) (simulate rgen rs s)
    writeObs fn obss traj
