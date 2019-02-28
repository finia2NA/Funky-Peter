module Reduction (findRule, reduceAt, reduciblePos, isNormalForm) where

import Matching
import Prog
import Term
import Subst
import Pos
import Util


-- returned die erste Regel, die man auf den Term anwenden kann,
-- oder Nothing wenn es keine solche Regel gibt.
findRule:: Prog -> Term -> Maybe(Rhs, Subst)
findRule (Prog rules) term = foldr reductor Nothing rules
 where
  reductor (Rule lh rh) acc =
    if notNothing acc then acc -- Skip rest if we already have a return value
    else
    if notNothing (match lh term)
      then Just (rh, (unwrap (match lh term)))
      else Nothing

-- given a term a and a program p, returns a term a' which was reduced at a given pos, 
-- or nothing if such a reduction was not possible with the given p.
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt prog term pos = let subterm = selectAt term pos in
  buildReturn (findRule prog subterm)
 where
  buildReturn :: Maybe(Rhs, Subst) -> Maybe Term
  buildReturn (Just (rh, subst)) = Just (replaceAt term pos (apply subst rh))
  buildReturn _ = Nothing

-- returns a list of reducible positions in the given term
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos prog term = filter isReduciblePos (allPos term)
 where
  isReduciblePos :: Pos -> Bool
  isReduciblePos pos = notNothing (findRule prog (selectAt term pos))

-- is the given term already reduced as much as possible given this program?
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog term = length (reduciblePos prog term) == 0 

-- Tests 
testProg1 = Prog [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"))]
testTerm1 = (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])
test1 f = f testProg1 testTerm1

testProg2 = Prog [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m")),
                  (Rule (Comb "add" [Comb "SUCC" [Var "n"], Var "m"]) (Comb "SUCC" [Comb "add" [Var "n", Var "m"]]))]
testTerm2 = (Comb "add" [(Var "n"), (Comb "add" [Comb "ZERO" [], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]])])