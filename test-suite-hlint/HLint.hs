module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "lint"
    , "src"
    , "src-ghc-8.10"
    , "src-ghc-8.8"
    , "src-ghc-8.6"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
