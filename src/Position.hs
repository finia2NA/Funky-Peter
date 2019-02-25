import Term
import Pretty

type Pos = Int

-- How is this suppose to work with (Var x)? What is the supterm of a variable.
-- Is this even supposed to work with vars?
-- Is the Pos just supossed to be the nth arg to a func?
-- What should happen if n is greater than the var count?
selectAt:: Term -> Pos -> Term
selectAt (Comb _ xs) n = xs !! n
