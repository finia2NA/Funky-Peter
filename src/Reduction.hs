import Matching
import Prog
import Term
import Subst

-- Suche Regel in Prog (Liste von Regeln) für die gilt:
--  - Linke Seite ist == Term
--  - Es existiert match zwischen term und rhs
      
-- findRule1 :: Prog -> Term -> Maybe(Rhs, Subst)
-- findRule1 prog term =
--   myReturn term (myFinder prog term)
--  where 
--   myReturn _ Nothing = Nothing
--   myReturn term (Just Rhs) = Just(Rhs, Subst.single(term, Rhs))

--   myFinder [] _ = Nothing
--   myFinder ((Rule Lhs Rhs):s) term
--     | unifiable term Lhs = Just Rhs
--     | otherwise = myFinder s term

--   unifiable specific general
--     | Matching.match general specific == Nothing = False
--     | otherwise = True

      
findRule2 :: Prog -> Term -> Maybe(Rhs, Subst)
findRule2 prog term = unwrap find filter (map mapper prog)
 where
  mapper (Rule lh rh ) = let susbst = maybe rh term in 
    if match lf term != Nothing && subst != Nothing
      then Just (rh, subst)
      else Nothing
  filter (Just _ _) = True
  filter _ = False
  unwrap :: Maybe a -> a
  unwrap (Just x) = x
  unwrap (Nothing) = Nothing


testRules = [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"))]
testTerm = (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])
        