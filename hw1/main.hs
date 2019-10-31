module Main where

-- 1st task
bell :: Int -> Integer
bell = head . (iterate (last >>= scanl (+)) [1] !!) . pred

-- 2nd task
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = (x : [y | (y : _) <- xss]) : transpose (xs : [ y | (_ : y) <- xss])

interleave :: [[a]] -> [a]
interleave = concat . transpose
