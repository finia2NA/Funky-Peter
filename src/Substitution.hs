-- ADT for subtitution from variables to terms

import Term

data Subst = Sub (VarName -> Term)

identity :: Subst
identity = Sub (\x -> Var x)

single:: VarName -> Term -> Subst
single v term = Sub (\x -> term)

compose:: Subst -> Subst -> Subst
compose s1 s2 = Sub (s1 . s2)

apply:: Subst -> Term -> Term
