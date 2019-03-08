module Reduction where

import Matching
import Pos
import Prog
import Subst
import Term
import Util

-- | Return the right side of the first rule which can be used together with the
-- returned substitution to reduce the given term.
findRule:: Prog -> Term -> Maybe(Rhs, Subst)
findRule (Prog rules) term = foldr reductor Nothing rules
 where
  reductor (Rule lh rh) acc =
    if Util.notNothing acc then acc -- Skip rest if already has return value
    else
    if Util.notNothing (applyableSubst)
      then Just (rh, (Util.unwrap (applyableSubst)))
      else Nothing
   where applyableSubst = Matching.match lh term

-- | Reduces a term at a given position using the given program. Returns a new
-- term with the position reduced or nothing if no reduction is possible at the
-- given position.
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt prog term pos = let subterm = Pos.selectAt term pos in
  buildReturn (findRule prog subterm)
 where
  buildReturn :: Maybe(Rhs, Subst) -> Maybe Term
  buildReturn (Just (rh, subst)) =
    Just (Pos.replaceAt term pos (Subst.apply subst rh))
  buildReturn _                  = Nothing

-- | Returns a list of reducible positions in a given term
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos prog term = filter isReduciblePos (Pos.allPos term)
 where
  isReduciblePos :: Pos -> Bool
  isReduciblePos pos = Util.notNothing (findRule prog (Pos.selectAt term pos))

-- | A given term is in normal form for a given program if it is no longer 
-- reducible.
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog term = null (reduciblePos prog term)