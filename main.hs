{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Token (float)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO ( stdout, hFlush )

data Input 
    = Exit 
    | MathExpr Double
    | VarAssign T.Text Double
    deriving (Show)

mathExpr :: Env -> Parser Double
mathExpr env = sp (i1 env <|> i2 env <|> i3 env)

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

type Env = [(T.Text, Double)]

i1 :: Env -> Parser Double
i1 env = sp (try (add' env) <|> try (sub' env) <|> i2 env <|> i3 env )

i2 :: Env -> Parser Double
i2 env = sp (try (mul' env) <|> try (div' env) <|> i3 env)

i3 :: Env -> Parser Double
i3 env = sp (
    sin' env <|> cos' env <|> 
    tan' env <|> sqrt' env <|> 
    par' env <|> (read <$> many1 (oneOf "-.0123456789")) <|>
    try (var' env))

var' :: Env -> Parser Double
var' env = do
    varName <- T.pack <$> many1 letter
    case lookup varName env of
        Just x -> return x
        Nothing -> parserFail $ "variable \""  ++ T.unpack varName ++ "\" was not found!"

par' :: Env -> Parser Double
par' env = do
    char '('
    x <- mathExpr env
    char ')'
    return x

sin' :: Env -> Parser Double
sin' env = do
    try $ string "sin("
    x <- mathExpr env
    char ')'
    return $ sin x

cos' :: Env -> Parser Double
cos' env = do
    try $ string "cos("
    x <- mathExpr env
    char ')'
    return $ cos x

tan' :: Env -> Parser Double
tan' env = do
    try $ string "tan("
    x <- mathExpr env
    char ')'
    return $ tan x

sqrt' :: Env -> Parser Double
sqrt' env = do
    try $ string "sqrt("
    x <- mathExpr env
    char ')'
    return $ sqrt x

add' :: Env -> Parser Double
add' env = do
    x1 <- i2 env
    char '+'
    x2 <- mathExpr env
    return $ x1 + x2

sub' :: Env -> Parser Double
sub' env = do
    x1 <- i2 env
    char '-'
    x2 <- mathExpr env
    return $ x1 - x2

mul' :: Env -> Parser Double
mul' env = do
    x1 <- i3 env
    char '*'
    x2 <- mathExpr env
    return $ x1 * x2

div' :: Env -> Parser Double
div' env = do
    x1 <- i3 env
    char '/'
    x2 <- mathExpr env
    if x2 == 0 
        then parserFail "Tried to divide by 0!" -- BUG: error message not propegated to user
        else return $ x1 / x2

assign :: Env -> Parser (T.Text, Double)
assign env = do
    varName <- try $ many1 letter
    try spaces
    char '='
    mE <- mathExpr env
    return (T.pack varName, mE)

parseInput :: Env -> Parser Input
parseInput env = 
    uncurry VarAssign <$> try (assign env) <|>
    (MathExpr <$> mathExpr env) <|> 
    (try (string "exit") >> return Exit)

mainLoop :: Env -> IO ()
mainLoop env = do
    putStr "-> "
    hFlush stdout
    input <- TIO.getLine
    print env
    let e = parse (parseInput env) "" input
    case e of
        Right ans -> case ans of
            Exit -> return ()
            MathExpr n -> do
                putStrLn $ show n ++ "\n"
                mainLoop env
            VarAssign lab val -> do
                TIO.putStrLn $ lab <> " = " <> (T.pack . show) val
                mainLoop $ (lab, val):env
        Left err -> do
            print err
            mainLoop env

main :: IO ()
main = mainLoop []

