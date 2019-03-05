module Pos where

import Term

-- Position of a sub-term in a term. Position is a list of indices. Each index
-- is equivalent to the 0 based nth Argument in the term above. This position
-- of the root is the empty list.
type Pos = [Int]

-- | Is pos1 above pos2? True if pos1 is a sub-list of pos2.
above:: Pos -> Pos -> Bool
above pos1 pos2 = length pos1 < length pos2 && pos1 == (take (length pos1) pos2)

-- | Is pos1 below pos2? True if pos2 is a sub-list of pos1.
below :: Pos -> Pos -> Bool
below pos1 pos2 = length pos2 < length pos1 && pos2 == (take (length pos2) pos1)

-- | Is pos1 left of pos2? True if one index is smaller than the same index in
-- pos2 and one position is not part of the other position.
leftOf :: Pos -> Pos -> Bool
leftOf (p1 : ps1) (p2 : ps2) = p1 < p2 || leftOf ps1 ps2
leftOf _          _          = False
  
-- | Is pos1 right of pos2? True if one index is greater than the same index in
-- pos2 and one position is not part of the other position.
rightOf :: Pos -> Pos -> Bool
rightOf (p1 : ps1) (p2 : ps2) = p1 > p2 || rightOf ps1 ps2
rightOf _          _          = False

-- | Returns a sub-term of a given term at the specified position.
selectAt :: Term -> Pos -> Term
selectAt term        []     = term
selectAt (Comb _ xs) (p:ps) = selectAt (xs !! p) ps
selectAt (Var _)     (_:_)  =
  error "error in POS: tried going deeper in a Variable, which is not possible."

-- | Switches out a sub-term at a given position.
replaceAt :: Term -> Pos -> Term -> Term
replaceAt _           []     rpl = rpl
replaceAt (Comb n sl) (p:ps) rpl = 
  let (x, (y: ys)) = splitAt p sl in
    Comb n (x ++ [replaceAt y ps rpl] ++ ys)
replaceAt (Var _) (_:_) _        =
  error "error in POS: tried going deeper in a Variable, which is not possible."

-- | Returns a list of all possible Positions within the Term.
allPos :: Term -> [Pos]
allPos (Var _)     = [[]]
allPos (Comb _ xs) = let li = (length xs) - 1 in -- li = lastIndex
   [[]] -- the path that ends here
   ++ [(a:s) | a <- [0 .. li], s <- (allPos (xs !! a))] -- Paths to all children