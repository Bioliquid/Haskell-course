module Main where

type Name = String
data Variable = Var Name Int
data Monomial = Mono Int Variable
data Polynomial = Poly [Monomial]

instance Num Monomial where
    (+) (Mono a (Var v e)) (Mono b (Var s t)) = if v == s && e == t 
        then Mono (a + b) (Var v e)
        else 

main = print $ (Mono 4 (Var "A" 1)) + (Mono 4 (Var "B" 1))