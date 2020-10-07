{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Chromar.Enriched.Syntax
    ( -- * Enriched Expressions
      -- $Er
      Er(..), SEr(..)
    , Nm
    , mkEr
    ) where

import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import "template-haskell" Language.Haskell.TH (Name, Pat)

import Chromar.Core (Time)
import Chromar.Multiset (Multiset, toList)

type Nm = String

-- $Er
-- Enriched expressions consist of ordinary expressions, fluents and
-- observables, and any combinations of them.

-- | The syntax of enriched expressions.
data SEr e
    = XExpr (Set Name) e
    | Time
    | When (SEr e) (SEr e) (SEr e)
    -- ^ A condition, when the first is true evaluate the second otherwise
    -- evaluate the third.
    | Repeat (SEr e) (SEr e)
    -- ^ Repeat every fluent, repeating over time, where the first enriched
    -- expression is the period of cycling and the second is the behaviour to
    -- be cycled.
    | Obs Pat Nm (SEr e) (SEr e)
    -- ^ A database-inspired observables expression.
    deriving (Show)

-- | The semantic of enriched expressions as a function from a set of
-- property states and time to a set of values.
newtype Er a b = Er { at :: Multiset a -> Time -> b }

mmod :: Real a => a -> a -> a
mmod n m
    | m <=0 = 0
    | otherwise = mod' n m

repeatEvery :: Er a Time -> Er a b -> Er a b
repeatEvery et er' =
    Er
    { at = \s t -> at er' s (mmod t (at et s t))
    }

when :: Er a Bool -> Er a b -> Er a (Maybe b)
when eb er' =
    Er
    { at =
        \s t ->
             if at eb s t
                 then Just (at er' s t)
                 else Nothing
    }

orElse :: Er a (Maybe b) -> Er a b -> Er a b
e1 `orElse` e2 =
    Er
    { at = \s t -> fromMaybe (at e2 s t) (at e1 s t)
    }

time :: Er a Time
time =
    Er
    { at = \_ t -> t
    }

obs :: (a -> Bool) -> Er a (a -> b -> b) -> Er a b -> Er a b
obs f comb i =
    Er
    { at = \s t -> aggregate (at comb s t) (at i s t) . select f $ s
    }

select :: (a -> Bool) -> Multiset a -> Multiset a
select f = filter (\(el, _) -> f el)

aggregate :: (a -> b -> b) -> b -> Multiset a -> b
aggregate f i s = foldr f i (toList s)

mkEr :: (Multiset a -> Time -> b) -> Er a b
mkEr f = Er { at = f }
