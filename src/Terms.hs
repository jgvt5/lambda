module Terms where

import           Data.Set

data Term = Var Char | App Term Term | Abst Char Term

data UTerm = UVar Int | UApp UTerm UTerm | UAbst UTerm

instance Show Term where
    show (Var c     ) = [c]
    show (App  t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
    show (Abst c  t ) = "(\\" ++ [c] ++ "." ++ show t ++ ")"

newtype Context = Context {getContext :: String }

removeNames :: Context -> Term -> UTerm
removeNames = undefined

restoreNames :: UTerm -> Term
restoreNames = undefined


freeVars :: Term -> Set Char
freeVars (Var c     ) = singleton c
freeVars (App  t1 t2) = union (freeVars t1) (freeVars t2)
freeVars (Abst c  t ) = freeVars t `difference` singleton c

contexts = Context . elems . freeVars
