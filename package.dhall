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
, ghc-options =
    [ "-Wall"
    , "-Werror"
    , "-Wincomplete-uni-patterns"
    , "-Wcompat"
    , "-Widentities"
    , "-Wredundant-constraints"
    , "-fhide-source-paths"
    ]
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
    , when =
        [ { condition = "impl(ghc >= 8.10.0)", source-dirs = "src-ghc-8.10" }
        , { condition =
              "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
          , source-dirs =
              "src-ghc-8.8"
          }
        , { condition =
              "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
          , source-dirs =
              "src-ghc-8.6"
          }
        ]
    }
, tests =
    { gdiff =
        { dependencies =
            [ "base", "chromar" ]
        , other-modules =
            [] : List Text
        , main =
            "gdiff.hs"
        , source-dirs =
            "models"
        , buildable =
            False
        }
    , model =
        { dependencies =
            [ "base", "chromar" ]
        , other-modules =
            [] : List Text
        , main =
            "model.hs"
        , source-dirs =
            "models"
        , buildable =
            False
        }
    , market =
        { dependencies =
            [ "base", "chromar", "random", "normaldistribution" ]
        , other-modules =
            [] : List Text
        , main =
            "Market.hs"
        , source-dirs =
            "models/market"
        , buildable =
            False
        }
    , plant =
        { dependencies =
            [ "base", "chromar" ]
        , other-modules =
            [] : List Text
        , main =
            "plant.hs"
        , source-dirs =
            "models"
        , buildable =
            False
        }
    , simple =
        { dependencies =
            [ "base", "chromar" ]
        , other-modules =
            [] : List Text
        , main =
            "simple.hs"
        , source-dirs =
            "models"
        }
    , germ =
        { dependencies =
            [ "base", "chromar", "text", "random", "normaldistribution" ]
        , other-modules =
            [ "SeedsModel.Env", "SeedsModel.Germ" ]
        , main =
            "germ.hs"
        , source-dirs =
            "models/seedsModel"
        , buildable =
            False
        }
    , utils =
        { dependencies =
            [ "base", "chromar", "text", "random", "normaldistribution" ]
        , other-modules =
            [ "SeedsModel.Env", "SeedsModel.Utils" ]
        , main =
            "utils.hs"
        , source-dirs =
            "models"
        , buildable =
            False
        }
    }
}
