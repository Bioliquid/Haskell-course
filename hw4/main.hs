module Main where

import Data.Function

-- Larionov Nikita R3496

class MyModulus a where
    modl :: a -> Int

instance MyModulus Int where
    modl = abs

instance MyModulus [a] where
    modl = length

instance MyModulus (a, b) where
    modl = const 2

data IntPara = Point Int Int

instance MyModulus IntPara where
    modl (Point x y) = on (+) abs x y

main = print . modl $ Point 3 4