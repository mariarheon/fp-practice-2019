module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}
-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


--Стандартные логические операции для двух переменных = AND, OR, XOR
--Для их реализации необходимо создать три различных типа

newtype PSetOr a = PSetOr{ containsOr :: (a -> Bool) }

instance Monoid (PSetOr a) where
    mempty = PSetOr (\x -> False)

instance Semigroup (PSetOr a) where
    (<>) (PSetOr a1) (PSetOr a2) = PSetOr (\x -> a1 x || a2 x)

newtype PSetAnd a = PSetAnd{ containsAnd :: (a -> Bool) }

instance Monoid (PSetAnd a) where
    mempty = PSetAnd (\x -> False)

instance Semigroup (PSetAnd a) where
    (<>) (PSetAnd a1) (PSetAnd a2) = PSetAnd (\x -> a1 x && a2 x)

newtype PSetXor a = PSetXor{ containsXor :: (a -> Bool) }

instance Monoid (PSetXor a) where
    mempty = PSetXor (\x -> False)

instance Semigroup (PSetXor a) where
    (<>) (PSetXor a1) (PSetXor a2) = PSetXor (\x -> (a1 x ||  a2 x) && not (a1 x && a2 x))

--
newtype OurRes a = OurRes( Bool -> a)

instance  Functor OurRes where
    fmap func (OurRes a) = OurRes(func . a)

--
instance  Functor PSetOr where
    fmap func (PSetOr a) = PSetOr(\x -> False)


