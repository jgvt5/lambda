module Main where

import           Data.Set
import           Text.Parsec
import           Terms
import           Parser

main :: IO ()
main = do
    filename <- getLine
    input    <- readFile filename
    putStrLn "excute"
    case parse term "" input of
        Left err -> print err
        Right x ->
            print $ restoreNames (getContext x) (removeNames (getContext x) x)
