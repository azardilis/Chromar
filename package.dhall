    let testopts = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]

in  let deps =
          [ "base"
          , "random == 1.1"
          , "parsec"
          , "template-haskell"
          , "haskell-src-meta"
          , "containers"
          , "multiset"
          ]

in  { name =
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
            deps
        , exposed-modules =
            [ "Chromar" ]
        , other-modules =
            [ "Chromar.Core"
            , "Chromar.Experiment"
            , "Chromar.MAttrs"
            , "Chromar.MRuleParser"
            , "Chromar.Multiset"
            , "Chromar.Enriched.Parse"
            , "Chromar.Enriched.Syntax"
            , "Chromar.Enriched.TH"
            , "Chromar.Enriched.Zip"
            , "Chromar.Rule.TH"
            ]
        , when =
            [ { condition =
                  "impl(ghc >= 8.10.0)"
              , source-dirs =
                  "src-ghc-8.10"
              , other-modules =
                  [ "Internal.RuleQuotes" ]
              }
            , { condition =
                  "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
              , source-dirs =
                  "src-ghc-8.8"
              , other-modules =
                  [ "Internal.RuleQuotes" ]
              }
            , { condition =
                  "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
              , source-dirs =
                  "src-ghc-8.6"
              , other-modules =
                  [ "Internal.RuleQuotes" ]
              }
            ]
        }
    , tests =
        { hlint =
            { dependencies =
                [ "base", "hlint", "chromar" ]
            , ghc-options =
                testopts
            , other-modules =
                [] : List Text
            , main =
                "HLint.hs"
            , source-dirs =
                "test-suite-hlint"
            }
        , doctest =
            { dependencies =
                deps # [ "doctest", "QuickCheck", "chromar" ]
            , ghc-options =
                testopts
            , main =
                "DocTest.hs"
            , source-dirs =
                [ "src", "test-suite-doctest" ]
            , when =
                [ { condition =
                      "impl(ghc >= 8.10.0)"
                  , source-dirs =
                      "src-ghc-8.10"
                  }
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
        , gdiff =
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
