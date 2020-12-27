{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token (float)
import Data.Functor ((<&>))
import Data.Bifunctor (first, bimap)
import qualified Data.Text as T

data Command 
    = Exit 
    | MathExpr Double 
    deriving (Show)

mathExpr :: Parser Double
mathExpr = sp (i1 <|> i2 <|> i3)

sp :: Parser a -> Parser a
sp p = spaces >> p >>= \x -> spaces >> return x

{-
Different infix numbers. 
Example: 
  1 + 2 * 3 = 1 + (2 * 3)
  1 + 2 * sqrt(3) = 1 + (2 * (sqrt(3)))

level 1: + | -
level 2: * | /
level 3: sin | cos | tan | sqrt
level 4: float
-}


i1 :: Parser Double
i1 = sp (try add' <|> try sub' <|> i2 <|> i3)

i2 :: Parser Double
i2 = sp (try mul' <|> try div' <|> i3)

i3 :: Parser Double
i3 = sp (
    sin' <|> cos' <|> 
    tan' <|> sqrt' <|> 
    par' <|> (read <$> many1 (oneOf ".0123456789")))

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

sqrt' :: Parser Double
sqrt' = do
    try $ string "sqrt("
    x <- mathExpr
    char ')'
    return $ sqrt x

add' :: Parser Double
add' = do
    x1 <- i2
    char '+'
    x2 <- mathExpr
    return $ x1 + x2

sub' :: Parser Double
sub' = do
    x1 <- i2
    char '-'
    x2 <- mathExpr
    return $ x1 - x2

mul' :: Parser Double
mul' = do
    x1 <- i3
    char '*'
    x2 <- mathExpr
    return $ x1 * x2

div' :: Parser Double
div' = do
    x1 <- i3
    char '/'
    x2 <- mathExpr
    if x2 == 0 
        then parserFail "Tried to divide by 0!" -- BUG: error message not propegated to user
        else return $ x1 / x2




main :: IO ()
main = putStrLn "Hello, Haskell!"
