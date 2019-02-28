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
    if notNothing acc then acc -- Skip rest if we already have a return value
    else
    if notNothing (match lh term) && notNothing (match rh term)
      then Just (rh, (unwrap (match rh term)))
      else Nothing
   where
    notNothing (Just _) = True
    notNothing _        = False


unwrap :: Maybe a -> a
unwrap (Just a) = a


testRules = Prog [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"))]
testTerm = (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])
        
      

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
     where
      unwrapJust Nothing = False
      unwrapJust (Just _) = True



testProg1 = Prog [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"))]
testTerm1 = (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])

test1 f = f testProg1 testTerm1
