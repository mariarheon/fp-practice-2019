module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure fo = FourOf fo fo fo fo
    (<*>) (FourOf a b c d) (FourOf a1 b1 c1 d1) = FourOf (a a1) (b b1) (c c1) (d d1)

instance Monad FourOf where
    return fo = FourOf fo fo fo fo 
    (>>=) (FourOf a b c d) f = FourOf (f1 (f a)) (f2 (f b)) (f3 (f c)) (f4 (f d))
        where
            f1 (FourOf a1 _ _ _) = a1
            f2 (FourOf _ b1 _ _) = b1
            f3 (FourOf _ _ c1 _) = c1
            f4 (FourOf _ _ _ d1) = d1

test = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y }