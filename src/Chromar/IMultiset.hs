module Chromar.IMultiset where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Chromar.Multiset

mset :: (Ord a) => [a] -> MultiSet a
mset = MS.fromList

mdiff :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
mdiff = MS.difference

mplus :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
mplus = MS.union

mperms :: Int -> Int -> Int
mperms n k
    | k > n = 0
    | otherwise = product (take k $ numsToOne n)
  where
    numsToOne n = enumFromThenTo n (n - 1) 1

mmults
    :: (Ord a)
    => MultiSet a -> MultiSet a -> Int
mmults m1 m2 =
    product
        [ mperms (MS.occur el m2) m
        | (el, m) <- MS.toOccurList m1 ]

