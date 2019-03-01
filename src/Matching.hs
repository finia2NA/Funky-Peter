module Matching (match) where

import Subst
import Term
import Pos

-- TODO:
-- 1. Find (Vars _) in t1 ✔
-- 2. Für jede Var: guck ob es die selbe position in t2 gibt ✔
-- 2.5. Bekomme term in t2 an der pos ✔
-- 3. ersetze diese var durch den entsprechenden austruck in t2 ✔

match :: Term -> Term -> Maybe Subst
match (Comb _ _)  (Var _)     = Nothing
match (Var v)     t           = Just (Subst.single v t)
match (Comb x xs) (Comb y ys) =
  if x == y && length xs == length ys
  -- custom for loop || map mapped to [Mabye Subst]
  then foldr composeMaybeSubst (Just Subst.identity)
    (map (\i -> match (xs !! i) (ys !! i)) [0 .. (length xs - 1)])
  else Nothing
   where
    composeMaybeSubst :: Maybe Subst -> Maybe Subst -> Maybe Subst
    composeMaybeSubst Nothing      acc        = Nothing
    composeMaybeSubst (Just subst) (Just acc) = Just (Subst.compose subst acc)
    composeMaybeSubst _            Nothing    = Nothing

--------------------- test code ---------------------

-- t11 = Comb "add" [Var "2", Var "3"]
-- t21 = Comb "add" [Comb "TWO" [], Comb "THREE" []]
-- test1 = apply (unwrap (match t11 t21)) t11

-- testvar1 = Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Var "m", Var "n"]]
-- testvar2 = Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Comb "Succ" [Comb "Zero" []], Comb "Succ" [Comb "Zero" []]]]
-- test2 = apply (unwrap (match testvar1 testvar2)) testvar1

-- testvar3 = Comb "add" [Comb "first" [Var "2", Var "3"], Comb "first" [Var "2", Var "3"]]
-- testvar4 = Comb "add" [Comb "first" [Comb "TWO" [], Comb "THREE" []], Comb "first" [Comb "TWO" [], Comb "THREE" []]]
-- test3 = apply (unwrap (match testvar3 testvar4)) testvar3


testT1 = Comb "add" [(Comb "ONE" []), (Comb "ONE" [])]
testT2 = Comb "mult" [(Comb "ONE" []), (Comb "ONE" [])]

testT3 = Comb "SUCC" [Comb "SUCC" [Comb "SUCC" [Var "m"]]]
testT4 = Comb "SUCC" [Comb "SU" [Comb "SUCC" [Comb "ZERO" []]]]

testV1 = Comb "SUCC" [(Comb "a" []), (Var "x")]
testV2 = Comb "PRE" [(Comb "a" []), (Comb "y" [])]