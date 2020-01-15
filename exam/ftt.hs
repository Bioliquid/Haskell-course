module Ftt where

import Data.Char

-- second task
dup :: [a] -> [a]
dup [] = []
dup (x : xs) = x : x : dup xs

-- third task
-- or {-# LANGUAGE FlexibleInstances #-} and {-# OVERLAPPING #-}
-- in case of "type Board = [[Bool]]"
data Board = Board [[Bool]]

toPic :: Bool -> Char
toPic True  = '█'
toPic False = '░'

instance Show Board where
    show (Board x) = unlines $ map (map (toPic)) x

main = print $ Board [[True, True, False, True], [False, True, True, False], [True, True, False, True], [False, True, True, False]]