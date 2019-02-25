-- I am so pretty!
module (Pretty(..)) where

import Term

class Pretty where
  pretty :: Term -> String
  pretty (Var a) = a
  pretty (Comb name []) = name
  pretty (Comb name xs) = name ++ "" ++ (foldl helper "" xs)
    where
    helper oldstring term = oldstring ++ " " ++ (pretty' term)
    pretty' x@(Var a) = pretty x
    pretty' x@(Comb name []) = pretty x
    pretty' (Comb name xs) = "(" ++ name ++ "" ++ (foldl helper "" xs) ++ ")"
  
-- -- I'm pretty too
-- pretty2 :: Term -> String
-- pretty2 (Var a) = a
-- pretty2 (Comb name []) = name
-- pretty2 (Comb name [x : []]) = 