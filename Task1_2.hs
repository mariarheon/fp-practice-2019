module Task1_2 where
import Todo(todo)
import Prelude hiding (sin, cos, gcd)

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y 
    | x < 0 || y < 0 = error("Argument should be greater than zero")
    | x == 0 = y
    | y == 0 = x
    | otherwise = gcd y (mod x y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = 
    if (day > 0) && (month > 0) && (month <= 12) && (year > 0) then
        case month of
            2 -> if isLeapYear year then
                    day <= 29
                  else day <= 28
            x | elem x [1, 3, 5, 7, 8, 10, 12] -> day <= 31
            x | elem x [4, 6, 9, 11] -> day <= 30
    else False

isLeapYear :: Integer -> Bool
isLeapYear year = 
    if (year `mod` 4 == 0) && 
        ((year `mod` 100 /= 0) || (year `mod` 400 == 0))
        then True
    else False
-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y 
    | y < 0 = error("Power should be equal or greater than zero")
    | y == 0 = 1
    | y == 1 = x
    | otherwise = if even y then x * pow x (y - 1)
                            else x * (pow (x * x) ((y - 1) `div` 2))
-- является ли данное число простым
isPrime :: Integer -> Bool
isPrime x = todo

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo