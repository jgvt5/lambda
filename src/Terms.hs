module Terms where

import           Data.Set

type Context = String
data Term = Var Char | App Term Term | Abst Char Term
data UTerm = UVar Int | UApp UTerm UTerm | UAbst UTerm

instance Show Term where
    show (Var c     ) = [c]
    show (App  t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
    show (Abst c  t ) = "(\\" ++ [c] ++ "." ++ show t ++ ")"

instance Show UTerm where
    show (UVar n    ) = show n
    show (UApp t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
    show (UAbst t   ) = "(\\." ++ show t ++ ")"

search :: Char -> Context -> Int -> Int
search _ [] _ = undefined
search vc (x : xs) n | vc == x   = n
                     | otherwise = search vc xs (n + 1)

removeNames :: Context -> Term -> UTerm
removeNames cs (Var c     ) = UVar (search c cs 0)
removeNames cs (App  t1 t2) = UApp (removeNames cs t1) (removeNames cs t2)
removeNames cs (Abst c  t ) = UAbst (removeNames (c : cs) t)

restoreNames :: Context -> UTerm -> Term
restoreNames cs (UVar n    ) = Var (cs !! n)
restoreNames cs (UApp t1 t2) = App (restoreNames cs t1) (restoreNames cs t2)
restoreNames cs (UAbst t   ) = Abst c (restoreNames (c : cs) t)
    where c = newVar cs

freeVars :: Term -> Set Char
freeVars (Var c     ) = singleton c
freeVars (App  t1 t2) = union (freeVars t1) (freeVars t2)
freeVars (Abst c  t ) = freeVars t `difference` singleton c

getContext = elems . freeVars

newVar :: Context -> Char
newVar c = head [ x | x <- ['a' .. 'z'], x `notElem` c ]
