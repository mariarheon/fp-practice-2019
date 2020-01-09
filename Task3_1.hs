module Task3_1 where

import Todo(todo)
{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance  Show WeirdPeanoNumber where
    show Zero       = "Zero"
    show (Succ wpn) = "Succ " ++ show wpn
    show (Pred wpn) = "Pred " ++ show wpn

instance Eq WeirdPeanoNumber where
    (==) wpn1 wpn2 = toInt wpn1 == toInt wpn2

instance Ord WeirdPeanoNumber where
    compare wpn1 wpn2 = compare' (normalize wpn1) (normalize wpn2)

instance Enum WeirdPeanoNumber where
    toEnum i     = fromInt (fromIntegral i)
    fromEnum wpn = fromIntegral (toInt wpn) 

instance Real WeirdPeanoNumber where
    toRational wpn = toRational (toInt wpn)

instance Integral WeirdPeanoNumber where
    toInteger wpn     = toInt wpn
    quotRem wpn1 wpn2 = quotRem' wpn1 wpn2

instance Num WeirdPeanoNumber where
    (+) wpn1 wpn2   = add wpn1 wpn2
    (*) wpn1 wpn2   = multiply wpn1 wpn2
    fromInteger wpn = fromInt wpn
    negate wpn      = negate' wpn
    abs wpn         = abs' wpn
    signum wpn      = signum' wpn


toInt :: WeirdPeanoNumber -> Integer
toInt Zero = 0
toInt (Succ wpn) = 1 + (toInt wpn)
toInt (Pred wpn) = (toInt wpn) - 1

fromInt :: Integer -> WeirdPeanoNumber
fromInt i | i > 0 = Succ (fromInt (i - 1))
          | i < 0 = Pred (fromInt (i + 1))
          | i == 0 = Zero

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize wpn = fromInt (toInt wpn)

compare' :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
compare' wpn1 wpn2 | toInt wpn1 > toInt wpn2 = GT
                   | toInt wpn1 < toInt wpn2 = LT
                   | toInt wpn1 == toInt wpn2 = EQ               
      

quotRem' :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
quotRem' _ Zero = error("Division by zero!")
quotRem' Zero _ = (Zero, Zero)
quotRem' wpn1 wpn2 = (quot, rem)
                  where quot = signTot * wpnQuot (abs wpn1) (abs wpn2) 
                        rem = (abs wpn1) - abs(wpn2 * quot)
                        signTot = signum wpn1 * signum wpn2
 
wpnQuot :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
wpnQuot wpn1 wpn2 | (wpn1 - wpn2) <= Zero = Zero
                  | otherwise = (quot (wpn1-wpn2) wpn2) + 1
                  
add :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
add wpn1 Zero = wpn1
add wpn1 (Succ wpn2) = Succ (wpn1 + wpn2)
add wpn1 (Pred wpn2) = Pred (wpn1 + wpn2)

negate' :: WeirdPeanoNumber -> WeirdPeanoNumber
negate' Zero = Zero
negate' (Succ wpn) = Pred (negate' wpn)
negate' (Pred wpn) = Succ (negate' wpn)

abs' :: WeirdPeanoNumber -> WeirdPeanoNumber
abs' wpn | wpn > 0 = wpn
         | wpn < 0 = negate' wpn
         | wpn == 0 = Zero

signum' :: WeirdPeanoNumber -> WeirdPeanoNumber
signum' Zero = Zero
signum' (Succ _) = Succ Zero
signum' (Pred _) = Pred Zero

multiply :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply wpn1 wpn2 | signum' wpn1 == signum wpn2 = mult (abs' wpn1) (toInt (abs' wpn2))
                   | otherwise = negate' ( mult (abs' wpn1) (toInt (abs' wpn2)))
            where
                mult x 0 = Zero
                mult x n = x + (mult x (n - 1))




