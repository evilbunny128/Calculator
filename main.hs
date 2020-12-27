module Main where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Token (float)
import Data.Functor ((<&>))
import Data.Bifunctor (first, bimap)
import qualified Data.Text as T

data Command 
    = Exit 
    | MathExpr Double 
    deriving (Show)

mathExpr :: Parser Double
mathExpr = spaces >> (
    par' <|> sin' <|> cos' <|> 
    tan' <|> sqrt <|> add' <|> 
    sub' <|> mul' <|> div' <|> 
    mod') >>= \x -> spaces >> return x

par' :: Parser Double
par' = do
    char '('
    x <- mathExpr
    char ')'
    return x

sin' :: Parser Double
sin' = do
    try $ string "sin("
    x <- mathExpr
    char ')'
    return $ sin x

cos' :: Parser Double
cos' = do
    try $ string "cos("
    x <- mathExpr
    char ')'
    return $ cos x

tan' :: Parser Double
tan' = do
    try $ string "tan("
    x <- mathExpr
    char ')'
    return $ tan x

main :: IO ()
main = putStrLn "Hello, Haskell!"
