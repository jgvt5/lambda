module Term where

data Term = Var Char | App Term Term | Abst Char Term

instance Show Term where
    show (Var c) = [c]
    show (App t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
    show (Abst c t) = "(\\" ++ [c] ++ "." ++ show t ++ ")"