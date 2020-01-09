module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции
Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

intPart :: Parser String
intPart = many1 digit

fracPart :: Parser String
fracPart = do
                char '.'
                res <- many1 digit
                return ('.' : res)

number :: Parser Double
number = do
            num <- intPart
            opt <- option "" fracPart
            return $ read (num ++ opt)

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser  (Double -> Double -> Double)
div_ = do
    char '/'
    return (/)

star :: Parser  (Double -> Double -> Double)
star = do
    char '*'
    return (*)

plus :: Parser  (Double -> Double -> Double)
plus = do
    char '+'
    return (+)

minus :: Parser  (Double -> Double -> Double)
minus = do
    char '-'
    return (-)

factorial :: Parser (Double -> Double)
factorial = do
    char '!'
    return fact

fact :: Double -> Double
fact x 
    | x < 0 = error("Argument should be greater than zero")
    | x == 0 = 1
    | (floor x == ceiling x) = error("Argument should be an integer")
    | otherwise = x * fact (x - 1)

negative :: Parser Double
negative = atom <|> do 
                    char '-'
                    res <- atom 
                    return (negate res)

factoriation :: Parser Double
factoriation = do
    spaces
    lhv <- negative
    spaces
    t <- many (factorial)
    return (foldl (\ lhv f -> f lhv) lhv t)

multiplication :: Parser Double
multiplication = do
    spaces
    lhv <- factoriation
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- star <|> div_
                spaces
                rhv <- factoriation
                spaces
                return (`f` rhv)

addition :: Parser Double
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- plus <|> minus
                spaces
                rhv <- multiplication
                spaces
                return (`f` rhv)

atom :: Parser Double
atom = number <|> do
    char '('
    res <- addition
    char ')'
    return res

main :: Parser Double
main = do
        spaces
        r <- addition
        eof
        return r