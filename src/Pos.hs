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

-- switches out a subterm at a given position.
replaceAt :: Term -> Pos -> Term -> Term
replaceAt src [] rpl = rpl
replaceAt (Comb n sl) (p:ps) rpl = let (x, (y: ys)) = splitAt p sl in
  Comb n (x ++ [(replaceAt y ps rpl)] ++ ys)

-- returns a list of all Possible Positions within the Term.
allPos :: Term -> [Pos]
allPos (Var v) = [[]]
allPos (Comb c xs) = let li = (length xs) - 1 in -- li = lastIndex
   [[]] -- the path that ends here
   ++ [(a:s) | a <- [0 .. li], s <- (allPos (xs !! a))] -- Paths to all children