import Matching
import Prog
import Term
import Subst
import Data.List

-- Suche Regel in Prog (Liste von Regeln) für die gilt:
--  - Linke Seite ist == Term
--  - Es existiert match zwischen term und rhs

findRule:: Prog -> Term -> Maybe(Rhs, Subst)
findRule (Prog rules) term = foldr reductor Nothing rules
 where
  reductor (Rule lh rh) acc =
    if maybeCompare (match lh term) && maybeCompare(match rh term)
      then if maybeCompare acc then acc else Just (rh, (unwrap (match rh term)))
      else if maybeCompare acc then acc else Nothing
  maybeCompare (Just _) = True
  maybeCompare _        = False


unwrap :: Maybe a -> a
unwrap (Just a) = a


testRules = Prog [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"))]
testTerm = (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])
        