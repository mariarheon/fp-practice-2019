module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap f (FunMonad fm) = FunMonad(\x -> f (fm x)) 

instance Applicative FunMonad where
    pure fm = FunMonad(\x -> fm)
    (<*>) (FunMonad fm1) (FunMonad fm2) = FunMonad(\x -> (fm1 x) (fm2 x))

instance Monad FunMonad where
    return fm = FunMonad(\x -> fm)
    (>>=) (FunMonad fm) f = FunMonad(\x -> fun (f (fm x) ) x)