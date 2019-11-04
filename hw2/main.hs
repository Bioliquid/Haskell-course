module Main where

import Control.Monad

-- first task
-- 1a)
type Graph = [(Int, [Int])]

hasElem :: Int -> Maybe [Int] -> Bool
hasElem src (Just xs) = elem src xs
hasElem _ _ = False

-- check that graph is not oriented
isCorrect :: Graph -> Bool
isCorrect xs = and [hasElem src (lookup dst xs) | (src, ys) <- xs, dst <- ys]

-- 1b)
-- all adjacent nodes
adjacent :: Graph -> Int -> [Int]
adjacent g node = case (lookup node g) of
                        Just xs -> xs
                        Nothing -> []

-- find all reachable nodes
bfs :: [Int] -> Graph -> Int -> [Int]
bfs used g u = u : (adjacent g u >>= \v -> guard (notElem v used) >> bfs (u : used) g v)

-- checking whether Node is reachable from the first node for every [(Node, _)]
isConnected :: Graph -> Bool
isConnected g@((node, _) : _) = null $ filter (flip notElem xs) (map fst g) where
                                xs = bfs [] g node

-- Second task
type Relation a = ((a, a) -> Bool)
type RelationProperty a = [a] -> Relation a -> Bool

isReflexive :: RelationProperty a
isReflexive xs rel = and [rel (x, x) | x <- xs]

isSymmetric :: RelationProperty a
isSymmetric xs rel = and [rel (y, x) | x <- xs, y <- xs, rel (x, y) == True]

isTransitive :: RelationProperty a
isTransitive xs rel = and [rel (x, z) | x <- xs, y <- xs, z <- xs, rel (x, y) == True, rel (y, z) == True]

isEquivalenceRelation :: RelationProperty a
isEquivalenceRelation xs rel = (isReflexive xs rel) && (isSymmetric xs rel) && (isTransitive xs rel) 

main = print $ "hello"