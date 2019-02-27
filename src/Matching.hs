import Subst
import Term
import Pos

-- TODO:
-- 1. Find (Vars _) in t1 ✔
-- 2. Für jede Var: guck ob es die selbe position in t2 gibt ✔
-- 2.5. Bekomme term in t2 an der pos ✔
-- 3. ersetze diese var durch den entsprechenden austruck in t2 ✔

-- Returns a list of substitutions from t1 to t2 if it is possible
match:: Term -> Term -> Maybe Subst
match t1 t2 
  | eap t1 t2 = Just (
    foldr Subst.compose Subst.identity (map (helper t1 t2) (findAllVars t1))
    )
  | otherwise = Nothing

helper :: Term -> Term -> Pos -> Subst
helper t1@(Var v) t2 pos = let newTerm = (selectAt t2 pos) in
  single v newTerm
  
{-
  given two terms t1 and t2,
  is there a term in t2 at the same position as every var in t1?
  
  AKA gilt 
  ∀var∈t1: ∃term∈t2: t1.pos=t2.pos
  ?
-}
eap :: Term -> Term -> Bool
eap t1 t2 = let baum = (Pos.allPos t2) in
  foldl (helper baum) True (findAllVars t1)
   where
    helper baum prevBool pfad  = prevBool && (elem pfad baum)

-- Returns the positions of all vars in a given term
findAllVars :: Term -> [Pos]
findAllVars t = filter (\p -> isVar (Pos.selectAt t p)) (Pos.allPos t)
 where
  isVar (Var _) = True
  isVar _ = False

--------------------- test code ---------------------

unwrap :: Maybe Subst -> Subst
unwrap (Just s) = s
unwrap (Nothing) = identity



t1 = Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Var "m", Var "n"]]
t2 = Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Comb "Succ" [Comb "Zero" []], Comb "Succ" [Comb "Zero" []]]]
test1 = apply (unwrap (match t1 t2)) t1