module Main where

import Data.Set
import Text.Parsec
import Terms
import Parser

main :: IO ()
main = do
    t <- getLine
    print "pumomoenguegegionmoe"
    case (parse term "" t) of
        Left err  -> print err
        Right x  -> printBeta x