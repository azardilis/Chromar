{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

import Chromar

-- Agent declarations
data Agent = A { x :: Int }
           deriving (Eq, Show)

$(return [])

-- Rules
r1 = [rule| A{x=x}, A{x=x'} --> A{x=x+1}, A{x=x'-1} @1.0 [x' > 0] |]
r2 = [rule| A{x=x}          --> A{x=x}, A{x=0} @1.0 [True] |]

--- Initial state
s = ms [A{x=5}, A{x=5}]

model =
    Model
    { rules = [r1, r2]
    , initState = s
    }
