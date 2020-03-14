module Terms where

import Data.Set

data Term = Var Char | App Term Term | Abst Char Term

instance Show Term where
    show (Var c) = [c]
    show (App t@(Abst _ _) (Var c)) = "(" ++ show t ++ ")" ++ [c]
    show (App t1 (Var c)) = show t1 ++ [c]
    show (App t1 t2) = show t1 ++ "(" ++ show t2 ++ ")"
    show (Abst c t) = "\\" ++ [c] ++ "." ++ show t


freeVars :: Term -> Set Char
freeVars (Var c) = singleton c
freeVars (App t1 t2) = union (freeVars t1) (freeVars t2)
freeVars (Abst c t) = freeVars t `difference` singleton c

containsVar :: Char -> Term -> Bool
containsVar c t = member c $ freeVars t

assign :: Char -> Term -> Term -> Term
-- y[x:=n]
assign x n t@(Var y)
    | x == y = n
    | otherwise = t
-- (t1 t2)[x:=n]
assign x n (App t1 t2) = App t1' t2'
    where t1' = assign x n t1
          t2' = assign x n t2
-- (\y.m)[x:=n]
assign x n t@(Abst y m)
    | x == y = t
    | x `containsVar` m && y `containsVar` n = 
        let z = head [ch | ch <- ['a'..'z'], not (ch `containsVar` n || ch `containsVar` m)]
            p = assign y (Var z) m
            q = assign x n p
        in Abst z q
    | otherwise = Abst y $ assign x n m

isNormal :: Term -> Bool
isNormal (App (Abst c t) t2) = False
isNormal (App t1 t2) = isNormal t1 && isNormal t2
isNormal (Abst _ t) = isNormal t
isNormal _ = True

beta1 :: Term -> Term
beta1 (App (Abst x m) n) = assign x n m
beta1 (App t1 t2)
    | not (isNormal t1) = beta1 t1
    | otherwise = beta1 t2
beta1 (Abst _ t) | not (isNormal t) = beta1 t

printBeta :: Term -> IO ()
printBeta t = do
    print t
    if isNormal t
    then return ()
    else printBeta $ beta1 t