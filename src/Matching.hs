module Matching where

import Subst
import Term

-- | Tries to find a substitution between to terms by comparing the
-- combination names at the same position and argument length. If it matches it
-- tries to match between the arguments at the same index recursively. A 
-- variable can always be substituted to a term however a combination can never
-- be substituted to a variable.
match :: Term -> Term -> Maybe Subst
match (Comb _ _)  (Var _)     = Nothing
match (Var v)     t           = Just (Subst.single v t)
match (Comb x xs) (Comb y ys) =
  if x == y && length xs == length ys
    -- custom for loop || map maps to [Maybe Subst]
    then foldr composeMaybeSubst (Just Subst.identity)
      (map (\i -> match (xs !! i) (ys !! i)) [0 .. (length xs - 1)])
    else Nothing
 where
  composeMaybeSubst :: Maybe Subst -> Maybe Subst -> Maybe Subst
  composeMaybeSubst Nothing      _          = Nothing
  composeMaybeSubst (Just subst) (Just acc) = Just (Subst.compose subst acc)
  composeMaybeSubst _            Nothing    = Nothing
