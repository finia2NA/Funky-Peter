module Subst where

import Term

data Subst = Subst (Term -> Term)

identity :: Subst
identity = Subst (\x -> x)

single:: VarName -> Term -> Subst
single v term = Subst (\x -> if x == (Var v) then term else x)

compose:: Subst -> Subst -> Subst
compose (Subst f1) (Subst f2) = Subst (f1 . f2)

apply:: Subst -> Term -> Term
apply (Subst sub) term = sub term

test1 = apply (single ("hi") (Var "ho")) (Var "hi")