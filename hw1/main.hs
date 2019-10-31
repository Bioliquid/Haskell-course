module Main where

-- 1st task
bell :: Int -> Integer
bell = head . (iterate (last >>= scanl (+)) [1] !!) . pred

-- simple implementation of 1st task
choose n k = product [x | x <- [k+1..n]] / product [x | x <- [1..n-k]]

bell'' _ 0 = 1
bell'' n k = (choose n k) * (bell' k) + bell'' n (k - 1)

bell' n = bell'' (n - 1) (n - 1)

-- 2nd task
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = (x : [y | (y : _) <- xss]) : transpose (xs : [ y | (_ : y) <- xss])

interleave :: [[a]] -> [a]
interleave = concat . transpose
