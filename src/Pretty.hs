-- I am so pretty...
-- I'm sexy and I know it

import Term

-- class Pretty where
pretty :: Term -> String
pretty (Var a) = a
pretty (Comb name []) = name
pretty (Comb name xs) = "(" ++ name ++ " " ++e  (foldr helper "" xs) ++ ")"
  where
  helper term oldstring = oldstring ++ (pretty term)