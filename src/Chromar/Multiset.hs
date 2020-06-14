module Chromar.Multiset
    ( Multiset
    , mults
    , plus
    , diff
    , toList
    , ms
    ) where

import Data.List (find)

type Multiset a = [(a, Int)]

frequencies :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
frequencies acc [] = acc
frequencies acc (x:xs) = frequencies (update acc) xs
    where
        update ((y, n):ys)
            | x == y = (y, n + 1) : ys
            | otherwise = (y, n) : update ys
        update [] = [(x, 1)]

ms :: (Eq a) => [a] -> Multiset a
ms = frequencies []

occur :: (Eq a) => a -> Multiset a -> Int
occur el m = case lookup el m of
    (Just n) -> n
    Nothing -> 0

perms :: Int -> Int -> Int
perms n k
    | k > n = 0
    | otherwise = product (take k $ numsToOne n)
    where
        numsToOne n' = enumFromThenTo n' (n' - 1) 1

binom :: Int -> Int -> Int
binom n k = perms n k `quot` (product $ numsToOne k) where
    numsToOne n' = enumFromThenTo n' (n' - 1) 1

mults :: (Eq a) => Multiset a -> Multiset a -> Int
mults m1 m2 = product [ binom (occur el m2) m | (el, m) <- m1 ]

diff :: (Eq a) => Multiset a -> Multiset a -> Multiset a
diff [] _ys = []
diff ((x, n):xs) ys =
    sub $ find (\(y, _) -> x == y) ys
    where
        sub (Just (_y, m))
            | n - m > 0 = (x, n - m) : (diff xs ys)
            | otherwise = diff xs ys
        sub Nothing = (x, n) : (diff xs ys)

plus :: (Eq a) => Multiset a -> Multiset a -> Multiset a
plus [] ys = ys
plus ((x, n):xs) ys =
    add $ findAndRemove [] (\(y, _) -> x == y) ys
    where
        add (Just (_y, m), ys') = (x, n + m) : (plus xs ys')
        add (Nothing, _) = (x, n) : (plus xs ys)

findAndRemove :: [a] -> (a -> Bool) -> [a] -> (Maybe a, [a])
findAndRemove acc _ [] = (Nothing, reverse acc)
findAndRemove acc p (x:xs)
    | p x = (Just x, (reverse acc) ++ xs)
    | otherwise = findAndRemove (x : acc) p xs

toList :: Multiset a -> [a]
toList = concatMap (\(el, n) -> replicate n el)
