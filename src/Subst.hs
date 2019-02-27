module Subst where

import Term

data Subst = Sub (Term -> Term)

identity :: Subst
identity = Sub (\x -> x)

single:: VarName -> Term -> Subst
single v term = Sub (\x -> if x == (Var v) then term else x)

compose:: Subst -> Subst -> Subst
compose (Sub f1) (Sub f2) = Sub (f1 . f2)

apply:: Subst -> Term -> Term
apply (Sub sub) term = sub term