-- I am so pretty!
module Pretty (Pretty(..)) where
  
import Term

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Var a) = a
  pretty (Comb name []) = name
  pretty (Comb name xs) = name ++ "" ++ (foldl helper "" xs)
    where
    helper oldstring term = oldstring ++ " " ++ (pretty' term)
    pretty' x@(Var a) = pretty x
    pretty' x@(Comb name []) = pretty x
    -- Put brackets arround inner function calls
    pretty' (Comb name xs) = "(" ++ name ++ "" ++ (foldl helper "" xs) ++ ")"