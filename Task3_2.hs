  
module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons lst val) = (rlistToList lst) ++ [val]

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (head:tail) = RCons(listToRList tail) head

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance (Eq a) => Eq (ReverseList a) where
    (==) RNil  RNil = True
    (==) _ RNil = False
    (==) RNil _ = False
    (==) (RCons lst1 val1) (RCons lst2 val2) = lst1 == lst 2 && val1 == val2

instance (Ord a) => Ord (ReverseList a) where
    compare rla rlb =
            case (rla, rlb) of
                (RNil, RNil) -> Eq
                (RNil, _)    -> LT 
                (_, RNil)    -> GT
                (rca, rcb)   -> compare' rca rcb
                    where 
                        compare' (RCons lst1 val1) (RCons lst2 val2) | val1 > val2 = GT
                                                                     | val1 < val2 = LT
                                                                     | val1 == val2 = compare lst1 lst2

instance (Show a) => Show (ReverseList a) where
    show RNil = []
    show (RCons RNil val) = show val
    show (RCons lst val) = show val ++ ", " ++ show lst

instance Monoid (ReverseList a where
    mempty = RNil
    mappend RNil lst = lst
    mappend lst RNil = lst
    mappend lst1 (RCons lst2 val) = RCons (mappend lst1 lst2) val

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons lst val) = RCons (fmap f lst) (f val)