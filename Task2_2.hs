module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x lst = 
    case lst of
        [] -> x
        (head:tail) -> foldl f (f x head) tail

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x lst =
    case lst of
        [] -> x
        (head:tail) -> f head (foldr f x tail)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x =
    case f x of
        Nothing -> []
        Just (a, b') -> a:(unfoldr f b')

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr (\x s -> (f x):s) [] lst 

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr (\x s ->
                    case x of
                        Just y -> y:s
                        Nothing -> s) [] lst
-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = snd $ foldr (\x (r, l) -> (r - 1, x !! r : l)) (n, []) lst
                        where n = length lst - 1

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = foldr(\x s ->
                             if f x
                                then s
                                else (x:s) ) [] lst

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool 
elem el lst = foldr(\x s ->
                    if (x == el) 
                        then True
                        else s) False lst

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr(\x ->
                                    if (x >= to)
                                        then Nothing
                                        else Just (x, x + step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = foldr(\x s -> x:s) lst2 lst1

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\x -> 
                        if null x
                             then Nothing 
                             else (Just (splitAt nI x))) lst 
                        where nI = fromIntegral n