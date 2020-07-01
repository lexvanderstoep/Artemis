module Grammar.Arithmetic.Expression where

data Expression =
    Add  Expression Expression
  | Sub  Expression Expression
  | Mult Expression Expression
  | Div  Expression Expression
  | Num  Int

evaluate :: Expression -> Int
evaluate (Add e1 e2)  = (evaluate e1) + (evaluate e2)
evaluate (Sub e1 e2)  = (evaluate e1) - (evaluate e2)
evaluate (Mult e1 e2) = (evaluate e1) * (evaluate e2)
evaluate (Div e1 e2)  = (evaluate e1) `div` (evaluate e2)
evaluate (Num n)      = n