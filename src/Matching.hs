import Subst
import Term

match:: Term -> Term -> Maybe Subst
match (Var x) term = Just (single x term)