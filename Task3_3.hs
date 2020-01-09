module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}
-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


--Стандартные логические операции для двух переменных = AND, OR, XOR
--Для их реализации необходимо создать три различных типа
--mempty представляет собой нейтральный элемент. Таким образом, зададим для логических поераций их нейтральные элементы
--mappend, по какой-то загадочной для меня причине, компилятор предлагает заменить Semigroup. Соответственно, в (<>) реализованы сами стандартные логические операции.

newtype PSetOr a = PSetOr{ containsOr :: (a -> Bool) }

instance Monoid (PSetOr a) where
    mempty = PSetOr (\x -> False) -- Нейтральный элемент дизъюнкции - Ложь

instance Semigroup (PSetOr a) where
    (<>) (PSetOr a1) (PSetOr a2) = PSetOr (\x -> a1 x || a2 x)

newtype PSetAnd a = PSetAnd{ containsAnd :: (a -> Bool) }

instance Monoid (PSetAnd a) where
    mempty = PSetAnd (\x -> True) -- Нейтральный элемент коньюнкции - Истина

instance Semigroup (PSetAnd a) where
    (<>) (PSetAnd a1) (PSetAnd a2) = PSetAnd (\x -> a1 x && a2 x)

newtype PSetXor a = PSetXor{ containsXor :: (a -> Bool) }

instance Monoid (PSetXor a) where
    mempty = PSetXor (\x -> False) -- Нейтральный элемент исключающего или - Ложь

instance Semigroup (PSetXor a) where
    (<>) (PSetXor a1) (PSetXor a2) = PSetXor (\x -> (a1 x ||  a2 x) && not (a1 x && a2 x))

-- Functor производит отображение элементов одного множества на элементы другого множества, так что
-- fmap :: ( a -> b) -> f a -> f b, где, в нашем случае,
-- аргументы a и b на самом деле аргументы функции ( a -> Bool ) и ( b -> Bool ).
-- То есть наш fmap будет выглядеть следующим образом:
-- ( ( a -> Bool ) -> ( b -> Bool )) -> f ( a -> Bool ) -> f ( b -> Bool ).
-- Просторы интернета и материалы лекций мне сообщили, что Haskell пока не умеет применять функцию к аргументу другой функции.
instance  Functor PSetOr where
    fmap func (PSetOr a) = PSetOr(\x -> False)


