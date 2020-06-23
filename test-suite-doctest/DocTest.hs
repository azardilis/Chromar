{-# LANGUAGE CPP #-}

-- WARNING: Using PackageImports because of the following error when running
-- doctest under cabal.
--  Ambiguous module name ‘Language.Haskell.TH’:
--    it was found in multiple packages:
--    ghc-lib-parser-8.10.1.20200523 template-haskell-2.14.0.0

module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "src/Chromar/Core.hs"
    , "src/Chromar/Enriched/Parse.hs"
    , "src/Chromar/Enriched/Syntax.hs"
    , "src/Chromar/Enriched/TH.hs"
    , "src/Chromar/Enriched/Zip.hs"
    , "src/Chromar/Experiment.hs"
    , "src/Chromar/MAttrs.hs"
    , "src/Chromar/MRuleParser.hs"
    , "src/Chromar/Multiset.hs"
    , "src/Chromar/Rule/TH.hs"

#if __GLASGOW_HASKELL__ >= 810
    , "-isrc-ghc-8.10"
    , "src-ghc-8.10/Internal/RuleQuotes.hs"
#elif __GLASGOW_HASKELL__ >= 808
    , "-isrc-ghc-8.8"
    , "src-ghc-8.8/Internal/RuleQuotes.hs"
#elif __GLASGOW_HASKELL__ >= 806
    , "-isrc-ghc-8.6"
    , "src-ghc-8.6/Internal/RuleQuotes.hs"
#endif

    , "-package random"

    , "-XPackageImports"
    ]

main :: IO ()
main = doctest arguments
