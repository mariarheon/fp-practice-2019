module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Ops = Add | Substract | Multiply deriving(Show,Eq)
data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op :: Ops, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l Add r
infixl 6 |+| 
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l Substract r
infixl 6 |-| 
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l Multiply r
infixl 7 |*| 

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
        case (expression) of
            Variable var | var == varName -> replacement
            BinaryTerm lhv op rhv -> BinaryTerm (replaceVar varName replacement lhv) op (replaceVar varName replacement rhv)  
            _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = 
    case expression of
        BinaryTerm lhv op rhv -> 
                case op of
                    Add -> IntConstant (leftV + rightV)
                    Substract -> IntConstant (leftV - rightV)
                    Multiply -> IntConstant (leftV * rightV)
                where
                    leftV = intValue (evaluate lhv)
                    rightV = intValue (evaluate rhv)
        Variable var -> error(var ++ " is not a constant")
        IntConstant var -> expression