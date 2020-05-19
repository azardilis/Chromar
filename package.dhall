{ verbatim =
    { cabal-version = ">= 1.8" }
, name =
    "chromar"
, version =
    "0.1.0.0"
, synopsis =
    "none"
, homepage =
    "none"
, author =
    "none"
, maintainer =
    "none"
, ghc-options =
    "-02"
, other-extensions =
    "BangPatterns"
, dependencies =
    [ "base", "containers" ]
, library =
    { source-dirs =
        "src"
    , dependencies =
        [ "base"
        , "random >=1.1 && <1.2"
        , "parsec >=3.1 && <3.2"
        , "template-haskell >=2.10 && <2.12"
        , "haskell-src-meta >= 0.6"
        , "containers >=0.5"
        , "multiset >= 0.3"
        ]
    , exposed-modules =
        [ "Chromar.Core"
        , "Chromar.RuleQuotesE"
        , "Chromar.Multiset"
        , "Chromar.MAttrs"
        , "Chromar.MRuleParser"
        , "Chromar.RExprs"
        , "Chromar.Experiment"
        , "Chromar"
        ]
    }
}
