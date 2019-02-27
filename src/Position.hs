import Term
import Pretty
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

type Pos = [Int]
-- is pos1 a function that uses pos2 as an argument?
above:: Pos -> Pos -> Bool
above pos1 pos2 = length pos1 < length pos2 && pos1 == (take (length pos1) pos2)

-- is pos2 an argument of pos1?
below :: Pos -> Pos -> Bool
below pos1 pos2 = length pos2 < length pos1 && pos2 == (take (length pos2) pos1)

-- Returns true if the first pos is left in the same term tree of pos2
leftOf :: Pos -> Pos -> Bool
leftOf pos1 pos2  = (init pos1 == init pos2) && (last pos1) < (last pos2)
  
-- Returns true if the first pos is right in the same term tree of pos2
rightOf :: Pos -> Pos -> Bool
rightOf pos1 pos2 = (init pos1 == init pos2) && (last pos1) > (last pos2)

selectAt :: Term -> Pos -> Term
selectAt term [] = term
selectAt (Comb _ xs) (p:ps) = selectAt (xs !! p) ps

replaceAt :: Term -> Pos -> Term -> Term
replaceAt src [] rpl = rpl
replaceAt (Comb n ss) (p:ps) rpl = let (x, (y: ys)) = splitAt p ss in
  Comb n (x ++ [(replaceAt y ps rpl)] ++ ys)

-- allPos :: Term -> [Pos]
-- allPos input = [[]] ++ (allPos' input)
--  where
--   allPos' (Var name) = []
--   allPos' (Comb Combname list) = (0 .. (length list))


-- paths :: Tree -> [[Int]]
-- paths Leaf = [[]]
-- -- paths (Node x Leaf Leaf) = [[x]]


-- -- paths (Node x left right) = map (x:) (paths left ++ paths right)
-- paths (Node x list) = map (x:) (foldl helper [[]] list)
--  where
--   helper oldinput listelement = oldinput ++ (paths listelement)