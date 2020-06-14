{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

import           Chromar

-- Agent declarations
data Agent = A { x :: Int }
           deriving (Eq, Show)

$(return [])

-- Rules
r1 = [rule| A{x=x}, A{x=y} --> A{x='x+1'}, A{x='y-1'} @'1.0' ['y > 0'] |]
r2 = [rule| A{x=x}          --> A{x='x'}, A{x='0'} @'1.0' |]

na = [er| select A{x=x}; aggregate (count . 'count + 1') '0' |]
nx = [er| select A{x=x}; aggregate (count . 'count + x') '0' |]

s = ms [A{x=5}, A{x=5}]

model =
    Model
    { rules = [r1, r2]
    , initState = s
    }

main =
    let nsteps = 100
    in run model nsteps (zipEr2 na nx)
