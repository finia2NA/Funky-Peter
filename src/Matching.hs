module Matching (match) where

import Subst
import Term
import Pos

-- TODO:
-- 1. Find (Vars _) in t1 ✔
-- 2. Für jede Var: guck ob es die selbe position in t2 gibt ✔
-- 2.5. Bekomme term in t2 an der pos ✔
-- 3. ersetze diese var durch den entsprechenden austruck in t2 ✔

-- Returns a list of substitutions from t1 to t2 if it is possible
match :: Term -> Term -> Maybe Subst
match t1 t2 
-- wenn es für jede Var ein subst geben wird,
-- dann return ein composed Subst von all diesen.
  | etav t1 t2 = Just (
  foldr Subst.compose Subst.identity (map (getSubst t1 t2) (findAllVars t1))
  )
  | otherwise = Nothing
  where
    -- gets the Substitution from t1.pos (which is a var) to t2.pos
  getSubst :: Term -> Term -> Pos -> Subst
  getSubst t1 t2 matchedPosition =
    let grabVar = (selectAt t1 matchedPosition)
        grabTerm = (selectAt t2 matchedPosition)
    in helper grabVar grabTerm
    where
      -- construct a Subst from a Variable and a Term.
      helper :: Term -> Term -> Subst
      helper (Var varname) term = single varname term


-- Returns a list of substitutions from t1 to t2 if it is possible
match2 :: Term -> Term -> Maybe Subst
match2 t1 t2 
  | etav t1 t2 = Just (
      foldr Subst.compose Subst.identity (
        map (\pos ->
          (\(Var v) -> Subst.single v (Pos.selectAt t2 pos))
          $ (Pos.selectAt t1 pos)
          )
          (findAllVars t1)
      )
    )
  | otherwise = Nothing

      {-
      given two terms t1 and t2,
  is there a term in t2 at the same position as every var in t1?
  
  AKA gilt 
  ∀var∈t1: ∃term∈t2: t1.pos=t2.pos
  ?
  -}
-- (etav stands for EXISTS TERM at ALL VARIABLES btw)
etav :: Term -> Term -> Bool
etav t1 t2 = let tree = (Pos.allPos t2) in
  foldl (existsPath tree) True (findAllVars t1)
   where
    existsPath tree' prevBool path  = prevBool && (elem path tree')

-- Returns the positions of all vars in a given term
findAllVars :: Term -> [Pos]
findAllVars t = filter (\p -> isVar (Pos.selectAt t p)) (Pos.allPos t)
 where
  isVar (Var _) = True
  isVar _ = False

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
testT4 = Comb "SUCC" [Comb "SUC" [Comb "SUCC" [Comb "ZERO" []]]]

testV1 = Comb "SUCC" [(Comb "a" []), (Var "x")]
testV2 = Comb "PRE" [(Comb "a" []), (Comb "y" [])]