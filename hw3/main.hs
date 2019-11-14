module Main where

-- Larionov Nikita R3496

import Data.List

-- first task
spigot :: Integral a => a -> a -> a -> a -> a -> a -> [a]
spigot q r t k n l = if 4 * q + r - t < n * t
                     then n : spigot (10 * q) (10 * (r - n * t)) t k (div (10 * (3 * q + r)) t - 10 * n) l
                     else spigot (q * k) ((2 * q + r) * l) (t * l) (k + 1) (div (q * (7 * k + 2) + r * l) (t * l)) (l + 2)

infPi :: Integral a => [a]
infPi = spigot 1 0 1 1 3 3

-- second task
factorials :: [Integer]
factorials = scanl (*) 1 [1..]

squares :: [Integer]
squares = [x * x | x <- [1..]]

cubes :: [Integer]
cubes = [x * x * x | x <- [1..]]

-- code from hoogle
-- Haskell, why Data.List.Utils is not avaliable?
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp [] ys = ys
mergeBy cmp xs [] = xs
mergeBy cmp (allx@(x : xs)) (ally@(y : ys))
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys

merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

-- Integer is Ord so nub is linear
getNth :: Int -> Integer
getNth = (!!) . nub $ merge factorials $ merge squares cubes

main = print $ getNth 5