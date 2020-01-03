module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil _ = error("List is empty")
index (DCons _ current _) 0 = current
index (DCons _ current right) i = index right (i - 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt DNil index value
         | index < 0  = error("Index should be equal or greater than zero")
         | index == 0 = DCons DNil value DNil
         | index > 0  = error("Not enough elements in the list")

insertAt (DCons left current right) index value 
         | index == 0 = let rec = DCons DNil value (insertAt' rec current right) in rec
         | index == 1 = let rec = DCons left current (insertAt' rec value right) in rec
         | index /= 1 =
              case right of 
                  DNil -> error("Not enough elements in the list")
                  otherwise   -> DCons left current (insertAt right (index - 1) value)

insertAt' :: DList a -> a -> DList a -> DList a
insertAt' left value DNil = DCons left value DNil
insertAt' left value (DCons _ value' right) = let rec = DCons left value (insertAt' rec value' right) in rec

removeAt :: DList a -> Int -> DList a
removeAt DNil _ = error("List is empty")
removeAt (DCons _ _ DNil) index
        | index == 0 = DNil
        | otherwise = error("Incorrect index value")
removeAt (DCons left current (DCons _ currentr rightr)) 0 = DCons left currentr rightr
removeAt (DCons left current right) index = DCons left current (removeAt right (index - 1))           
