module Parser where

import           Terms
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Pos

var :: Parser Term
var = do
    c <- oneOf ['a' .. 'z']
    return $ Var c

abst :: Parser Term
abst = do
    char '('
    char '\\'
    c <- oneOf ['a' .. 'z']
    char '.'
    t <- term
    char ')'
    return $ Abst c t

app :: Parser Term
app = do
    char '('
    t <- term
    s <- term
    char ')'
    return $ App t s

term :: Parser Term
term = var <|> try abst <|> app
