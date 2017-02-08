module Chromar.Fluent
    ( Time,
      Fluent,
      at,
      mkFluent,
      (<>*>),
      (<<*>),
      (<+*>),
      (<-*>),
      constant,
      time,
      orElse,
      when,
      after,
      between,
      fconcat,
      flookup,
      flookupM,
      every,
      empty,
      repeatEvery,
      acc,
      accTable
    ) where

import Control.Applicative hiding (empty)
import Data.Fixed
import qualified Data.Map.Strict as Map
import Data.Maybe

type Time = Double

data Fluent a = Fluent { at :: Time -> a }


instance Functor Fluent where
  fmap f fa = Fluent { at = f . at fa } 


instance Applicative Fluent where
    pure = constant
    (<*>) ff fa =
        Fluent
        { at = \t -> ($) (at ff t) (at fa t)
        }

--- maybe Monoid instance if 'a' is Monoid? Don't know if that's useful but maybe
instance Monoid a =>
         Monoid (Fluent a) where
    mempty = constant mempty
    mappend f f' =
        Fluent
        { at = \t -> at f t `mappend` at f' t
        }

-- Maybe give some of these for convenience otherwise make Fluents
-- instances of Num, Ord etc.
(<>*>)
    :: (Ord a)
    => Fluent a -> Fluent a -> Fluent Bool
(<>*>) = liftA2 (>)

(<<*>)
    :: (Ord a)
    => Fluent a -> Fluent a -> Fluent Bool
(<<*>) = liftA2 (<)

(<+*>)
    :: (Num a)
    => Fluent a -> Fluent a -> Fluent a
(<+*>) = liftA2 (+)

(<-*>)
    :: (Num a)
    => Fluent a -> Fluent a -> Fluent a
(<-*>) = liftA2 (-)

mkFluent :: (Time -> a) -> Fluent a
mkFluent tf = Fluent { at = tf }

empty :: Fluent ()
empty = Fluent { at = \t -> () }

constant :: a -> Fluent a
constant x = Fluent { at = const x }

time :: Fluent Time
time = Fluent { at = id }

funtilT :: Time -> Fluent a -> Fluent a -> Fluent a
funtilT t' fa fb = Fluent { at = \t -> if t <= t' then at fa t
                                       else at fb t }

orElse :: Fluent (Maybe a) -> Fluent a -> Fluent a
fa `orElse` fb = Fluent { at = \t -> case (at fa t) of
                             Just x  -> x 
                             Nothing -> at fb t }
                                                        
when :: Fluent Bool -> Fluent a -> Fluent (Maybe a)
when fb fa = Fluent { at = \t-> if at fb t then Just (at fa t)
                                else Nothing }

after :: Time -> Fluent Bool
after t' = time <>*> (constant t')

between :: Time -> Time -> Fluent a -> Fluent a -> Fluent a
between t1 t2 f1 f2 = funtilT t2 (funtilT t1 f2 f1) f2
                 
fconcat
    :: (Monoid a)
    => [Time] -> [Fluent a] -> Fluent a
fconcat ts fs =
    let emptyF = constant mempty
        emptyUntil = funtilT 0
        untils = map funtilT ts
    in fconcat' (emptyUntil : untils) fs emptyF

fconcat' :: [Fluent a -> Fluent a -> Fluent a]
         -> [Fluent a]
         -> Fluent a
         -> Fluent a
fconcat' [] [] acc = acc
fconcat' [] _ acc = acc
fconcat' _ [] acc = acc
fconcat' (f:fs) (v:vs) acc = fconcat' fs vs (f acc v)

-- assume ordered sequence and do binary instead of linear search
-- we are assuming it anyway so we might as well
ffind :: [(Time, a)] -> Time -> a
ffind [] _ = error ""
ffind [(t', v)] t = v
ffind ((t1, val1):(t2, val2):tvals) t =
    if (t >= t1 && t < t2)
        then val1
        else ffind ((t2, val2) : tvals) t

flookup :: [(Time, a)] -> Fluent a
flookup tvals = Fluent { at = ffind tvals }

flookupM
    :: (Fractional a)
    => Map.Map Time a -> Fluent a
flookupM tvals =
    Fluent
    { at = \t -> fromMaybe 0.0 $ fmap snd (Map.lookupLE t tvals)
    }

every :: Time -> a -> (a -> a) -> Fluent a
every t init acc =
    let times = scanl1 (+) (0 : repeat t)
        vals = iterate acc init
    in flookup (zip times vals)

repeatEvery :: Time -> Fluent a -> Fluent a
repeatEvery t' fa = Fluent { at = \t -> at fa (mod' t t') }

acc :: Fluent a -> Time -> b -> (a -> b -> b) -> Fluent b
acc f ti init acc =
    let times = scanl1 (+) (repeat ti)
        vals = map (at f) times
    in accTable (zip times vals) init acc
                      
accTable :: [(Time, a)] -> b -> (a -> b -> b) -> Fluent b
accTable table init accF =
    Fluent
    { at =
        \t -> foldr (\(tt, v) acc -> accF v acc) init (tableUntilTime t table)
    }
  where
    tableUntilTime t = takeWhile (\(tt, v) -> tt <= t)
