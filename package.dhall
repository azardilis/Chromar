{ name =
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
, other-extensions =
    "BangPatterns"
, dependencies =
    [ "base", "containers" ]
, library =
    { source-dirs =
        "src"
    , dependencies =
        [ "base"
        , "random"
        , "parsec"
        , "template-haskell"
        , "haskell-src-meta"
        , "containers"
        , "multiset"
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
