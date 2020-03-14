module Main where

import Data.Set
import Terms

main :: IO ()
main = do
    let x = Abst 'x' $ App (Var 'x')(Var 'x')
        y = Abst 'x' $ App (Var 'x')(Var 'x')
        z = App x y
    print $ freeVars x
    print $ beta1 z