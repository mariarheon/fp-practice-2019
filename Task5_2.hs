module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

instance (Show a) => Show (Zipper a) where
    show  = show . toList

instance (Eq a) => Eq (Zipper a) where
    (==) zip1 zip2 = toList zip1 == toList zip2 

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

toList :: Zipper a -> [a]
toList (Zipper left right) = concat [reverse (left) , right]

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concatZipper :: Zipper a -> Zipper a -> Zipper a
concatZipper left right = Zipper (toList left) (toList right)

-- assuming numeration starts with 1
insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index (Zipper [] []) into = into
insertManyAt index what (Zipper [] []) = what
insertManyAt index (Zipper lefts rights) (Zipper leftt rightt) 
                    | index <= 0 || index > len = error("Index should be greater than zero and less than the target zipper")
                    | index == 1 = concatZipper what into
                    | otherwise = insertManyAt (index - 1) what (goRight into)
                         where len =  length(concat[leftt, rightt])   
                               what = Zipper lefts rights
                               into = Zipper leftt rightt

-- [from, to]
subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to (Zipper [] []) = error("Zipper is empty")
subZipper from to (Zipper left right)
                    | from <= 0 || to <= 0 = error("Borders should be greater than zero")
                    | from > len || to > len = error("Borders should be  equal or less than the length of the zipper")
                    | from > to = error("(To) value should be equal or greater than (From) value")
                    | otherwise = fromList( take (to - from + 1) (drop (from - 1) ( toList (Zipper left right) ) ) )               
                        where len =  length(concat[left, right])

a = [1,2,3,4,5]
b = [6,7,8,9,0]
c = Zipper a b
