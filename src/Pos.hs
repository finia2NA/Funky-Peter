module Pos where

import Term

type Pos = [Int]

-- is pos1 a function that uses pos2 as an argument?
above:: Pos -> Pos -> Bool
above pos1 pos2 = length pos1 < length pos2 && pos1 == (take (length pos1) pos2)

-- is pos2 an argument of pos1?
below :: Pos -> Pos -> Bool
below pos1 pos2 = length pos2 < length pos1 && pos2 == (take (length pos2) pos1)

-- is pos1 left of pos2?
leftOf :: Pos -> Pos -> Bool
leftOf pos1 pos2  = (init pos1 == init pos2) && (last pos1) < (last pos2)
  
-- is pos1 right of pos2?
rightOf :: Pos -> Pos -> Bool
rightOf pos1 pos2 = (init pos1 == init pos2) && (last pos1) > (last pos2)

-- returns the subterm at a given position
selectAt :: Term -> Pos -> Term
selectAt term [] = term
selectAt (Comb _ xs) (p:ps) = selectAt (xs !! p) ps
selectAt (Var _) (_:_) = error "error in POS: you tried going deeper in a Variable, which is not possible"

-- switches out a subterm at a given position.
replaceAt :: Term -> Pos -> Term -> Term
replaceAt _ [] rpl = rpl
replaceAt (Comb n sl) (p:ps) rpl = let (x, (y: ys)) = splitAt p sl in
  Comb n (x ++ [(replaceAt y ps rpl)] ++ ys)
replaceAt (Var _) (_:_) _ = error "error in POS: you tried going deeper in a Variable, which is not possible"

-- returns a list of all Possible Positions within the Term.
allPos :: Term -> [Pos]
allPos (Var _) = [[]]
allPos (Comb _ xs) = let li = (length xs) - 1 in -- li = lastIndex
   [[]] -- the path that ends here
   ++ [(a:s) | a <- [0 .. li], s <- (allPos (xs !! a))] -- Paths to all children