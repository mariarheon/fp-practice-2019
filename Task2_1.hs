module Task2_1 where
import Prelude hiding (lookup)

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty | Node (Integer, v) (TreeMap v) (TreeMap v) deriving (Eq, Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k =
    case t of
        Empty -> False
        Node (key, _) left right 
            | key == k -> True
            | key > k -> contains right k
            | key < k -> contains left k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = 
    case t of
        Empty -> error("No such key")
        Node (key, value) left right 
            | key == k -> value
            | key > k -> lookup k right
            | key < k -> lookup k left

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = 
    case t of
        Empty -> Node (k, v) Empty Empty
        Node (key, value) left right 
            | key == k -> Node (k, v) left right
            | key > k -> Node (k, v) (insert (k, v) left) right
            | key < k -> Node (k, v) left (insert (k, v) right)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = 
    case t of
        Empty -> Empty
        Node (key, value) left right
            | key > i -> Node (key, value) (remove i left) right
            | key > i -> Node (key, value) left (remove i right)
            | key == i ->
                case (left, right) of
                    (Empty, Empty) -> Empty
                    (left, Empty) -> left
                    (Empty, right) -> right
                    (left, right) -> remove' left right

remove' :: TreeMap v -> TreeMap v -> TreeMap v
remove' left right = 
        case right of 
            Empty -> left
            Node (key, value) left' right' ->
                case left' of
                    Empty -> Node (key, value) left right'
                    otherwise -> Node (key, value) (remove' left left') right'

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = 
    case t of 
        Empty -> error("Not found")
        Node (key, value) left right 
            | key == i -> (key, value)
            | key > i -> nearestLE i left
            | key < i ->
                case right of
                    Node (key, value) _ _
                        | i == key -> (key, value)
                        | i /= key -> nearestLE i right
                    Empty -> (key, value)


-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = 
    case t of
        Empty -> []
        Node (key, value) left right 
              -> listFromTree left ++ [(key, value)] ++ listFromTree right
-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = 
    case t of
        Empty -> error("Not found")
        Node (key, value) left right
            | treeSize left == i -> (key, value)
            | treeSize left > i -> kMean i left
            | treeSize left < i -> kMean (i - 1 - treeSize left) right

treeSize :: TreeMap v -> Integer
treeSize t = 
    case t of
        Empty -> 0
        Node (_, _) left right -> (treeSize left) + 1 + (treeSize right)
