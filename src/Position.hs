import Term
import Pretty

type Level = Int
type Index = Int

data Pos = Pos Level Index

above:: Pos -> Pos -> Bool
below:: Pos -> Pos -> Bool
leftOf:: Pos -> Pos -> Bool
rightOf:: Pos -> Pos -> Bool

selectAt:: Term -> Pos -> Term

replaceAt:: Term -> Pos -> Term -> Term

allPos:: Term -> [Pos]

-- =============================================================================
-- Examples:

-- Var "m"
-- Var "m" => (0, 0)

-- Comb "Succ" [Comb "Zero" []]
-- Comb "Succ" => (0, 0)
--        |
--        |
-- Comb "Zero" => (1, 0)

-- Comb "Succ" [Comb "Zero" []], Comb "mul" [Var "m", Var "n"]]
--               Comb "Succ" => (0, 0)
--               /         \
--              /           \
--  Comb "Zero" => (1, 0)   Comb "mul" => (1, 1)
--                          /         \
--                         /           \
--               Var "m" => (2, 0)     Var "n" => (2, 1)

-- =============================================================================