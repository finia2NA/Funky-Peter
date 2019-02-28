import Matching
import Prog
import Term
import Subst 

-- Suche Regel in Prog (Liste von Regeln) für die gilt:
--  - Linke Seite ist == Term
--  - Es existiert match zwischen term und rhs
      
findRule1 :: Prog -> Term -> Maybe(Rhs, Subst)
findRule1 (Prog prog) term =
  myReturn term (myFinder prog term)
   where 

    myReturn _ Nothing = Nothing
    myReturn (Var v) (Just rhs) = Just(rhs, (Subst.single v rhs))

    myFinder [] _ = Nothing
    myFinder ((Rule lhs rhs):s) term
      | unifiable term lhs = Just rhs
      | otherwise = myFinder s term

    unifiable specific general = unwrapJust (Matching.match general specific)

    unwrapJust Nothing = False
    unwrapJust (Just _) = True



testRules = [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"))]
testTerm = (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])
        