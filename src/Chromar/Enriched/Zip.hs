module Chromar.Enriched.Zip (erZip, erZip3, erZip4, erZip5) where

import Chromar.Enriched.Syntax (Er(..))

erZip :: Er a b -> Er a c -> Er a (b, c)
erZip e1 e2 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t)
    }

erZip3 :: Er a b -> Er a c -> Er a d -> Er a (b, c, d)
erZip3 e1 e2 e3 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t)
    }

erZip4 :: Er a b -> Er a b1 -> Er a b2 -> Er a b3 -> Er a (b, b1, b2, b3)
erZip4 e1 e2 e3 e4 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t, at e4 s t)
    }

erZip5
    :: Er a b
    -> Er a b1
    -> Er a b2
    -> Er a b3
    -> Er a b4
    -> Er a (b, b1, b2, b3, b4)
erZip5 e1 e2 e3 e4 e5 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t, at e4 s t, at e5 s t)
    }

