module Subst where

import Term

data Subst = Subst (Term -> Term)

-- A Substitution that, when applied, doesn't change the term at all.
identity :: Subst
identity = Subst (\x -> x)

-- A Substitution that, when applied, replaces a given variable with a term.
single:: VarName -> Term -> Subst
single v term = Subst (\x -> if x == (Var v) then term else x)

-- given 2 substitutions s1 and s2, returns a substitution which, when applied,
-- produces the same term as applying s2 and s1.
compose:: Subst -> Subst -> Subst
compose (Subst f1) (Subst f2) = Subst (f1 . f2)

-- applies a substitution to a term
apply:: Subst -> Term -> Term
apply (Subst sub) t@(Var _) = sub t
apply sub (Comb n xs) = Comb n (map (apply sub) xs)

-- a show instance for substitutions so QuickCheck works
instance Show Subst where
  show (Subst _) = "hier steht ein subst"