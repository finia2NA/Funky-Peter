import Term
import Pretty

type Level = Int
type Index = Int
data Pos = Pos Level Index

-- -- How is this suppose to work with (Var x)? What is the supterm of a variable.
-- -- Is this even supposed to work with vars?
-- -- Is the Pos just supossed to be the nth arg to a func?
-- -- What should happen if n is greater than the var count?
-- selectAt:: Term -> Pos -> Term
-- selectAt (Comb _ xs) n = xs !! n

-- -- I am assuming that the position means the index in a subterm at a depth of 0.
-- -- Meaning we do not dive recursivly into sub sub terms.
-- -- This makes the most sence for me considering that we want to use this functionality
-- -- when evaluting terms.
-- replaceAt:: Term -> Pos -> Term -> Term
-- replaceAt t@(Comb _ []) _ _  = t
-- replaceAt (Comb name xs) n term = Comb name (replaceSub xs n term)
--   where
--     replaceSub:: [Term] -> Pos -> Term -> [Term]
--     replaceSub [] _ _ = []
--     replaceSub (_:xs) 0 term = term : xs
--     replaceSub (x:xs) n term = x : (replaceSub xs (n-1) term)

-- -- ?!?! Nani. Behaviour looks a bit strange to me... This is fucking stupid :(
-- allPos:: Term -> [Pos]
-- allPos (Comb _ xs) = take (length xs) [0..]
