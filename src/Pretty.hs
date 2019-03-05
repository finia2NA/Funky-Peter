module Pretty (Pretty(..)) where
  
import Term

-- | Displays a given thing in a manner that is visually pleasing.
class Pretty a where
  pretty :: a -> String

-- | Defines a pretty instance for Term
instance Pretty Term where
  pretty (Var x)        = x
  pretty (Comb name []) = name
  pretty (Comb name xs) = name ++ (foldl reducer "" xs)
   where
    reducer oldstring term   = oldstring ++ " " ++ (pretty' term)
    
    pretty' x@(Var _)        = pretty x
    pretty' x@(Comb _ [])    = pretty x
    -- Put brackets around inner function calls
    pretty' (Comb name' xs') = "(" ++ name' ++ (foldl reducer "" xs') ++ ")"