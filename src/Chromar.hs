{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Chromar
    ( module Chromar.Core
    , module Chromar.Enriched.Syntax
    , module Chromar.Enriched.TH
    , module Chromar.Enriched.Zip
    , module Chromar.Experiment
    , module Chromar.Multiset
    , module Chromar.Rule.TH
    ) where

-- WARNING: For now, we're only exporting what is needed by the test suites.
import Chromar.Core (Model(..), Time, Rxn(..), fullRate, nrepl)
import Chromar.Enriched.Syntax (Er(..), mkEr)
import Chromar.Enriched.TH (er)
import Chromar.Enriched.Zip (erZip, erZip3)
import Chromar.Experiment (run)
import Chromar.Multiset (Multiset, ms, toList)
import Chromar.Rule.TH (rule)
