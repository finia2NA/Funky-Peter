module Evaluation (
  Strategy,
  loStrategy,
  liStrategy,
  riStrategy,
  roStrategy,
  piStrategy,
  poStrategy,
  evaluateWith
) where

import Reduction
import Pos
import Term
import Prog
import Util
import qualified Data.List as List

{-
WIKI:
innermost:
  Die Argumente eines Funktionsaufsrufs werden
  VOR der Funktionsanwendung ausgewertet.
  Wie in imperativen Sprachen: f(g(x)) -> g wird vor f ausgeführt

outermost:
  Funktionen werden von aussen nach innen ausgeführt
  (substituiert). Diese art Auszuwerten ist _mächtiger_ als innermost,
  da das Programm zb bei ```f x = 1 | h = h``` für den Aufruf ```f h``` terminiert
  Es ist allerdings nicht vereinbar mit seiteneffektbehafteten Funktionen und
  somit nur in funktionalen Sprachen praktikabel.

left vs right:
  Bei aufrufen wie ```((square 2) + (square 5))``` gibt es mehrere innermost-
  Positionen. Nur zu sagen, dass man eine Innermost-Strategie verfolgt ist also
  nicht genug, um eine Strategie zu definieren. man muss noch sagen, ob man
  gleichgestellte Argumente von links oder von rechts durchgeht.

parallel:
  Links und Rechts gleichzeitig auswerten.

-}


-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

-- left-outermost
loStrategy :: Strategy
loStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy loOrdering (reduciblePos prog term))) : []
  )

loOrdering :: Pos -> Pos -> Ordering
loOrdering pos1 pos2
  | leftOf  pos1 pos2 = LT
  | rightOf pos1 pos2 = GT
  | above   pos1 pos2 = LT
  | below   pos1 pos2 = GT
  | otherwise = error "error found by strategy ™"

-- left-innermost
liStrategy :: Strategy
liStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy liOrdering (reduciblePos prog term))) : []
  )

liOrdering :: Pos -> Pos -> Ordering
liOrdering pos1 pos2
  | leftOf  pos1 pos2 = LT
  | rightOf pos1 pos2 = GT
  | below   pos1 pos2 = LT
  | above   pos1 pos2 = GT
  | otherwise = error "error found by strategy ™"

-- right-outermost
roStrategy :: Strategy
roStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy roOrdering (reduciblePos prog term))) : []
  )

roOrdering :: Pos -> Pos -> Ordering
roOrdering pos1 pos2
  | rightOf pos1 pos2 = LT
  | leftOf  pos1 pos2 = GT
  | above   pos1 pos2 = LT
  | below   pos1 pos2 = GT
  | otherwise = error "error found by strategy ™"

-- right-innermost
riStrategy :: Strategy
riStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy riOrdering (reduciblePos prog term))) : []
  )

riOrdering :: Pos -> Pos -> Ordering
riOrdering pos1 pos2
  | rightOf pos1 pos2 = LT
  | leftOf  pos1 pos2 = GT
  | below   pos1 pos2 = LT
  | above   pos1 pos2 = GT
  | otherwise = error "error found by strategy ™"


-- parallel outermost
poStrategy :: Strategy
poStrategy = (\prog term -> getMinPosis (reduciblePos prog term))
 where
  getMinPosis :: [Pos] -> [Pos]
  getMinPosis [] = []
  getMinPosis ps = let mini = head (List.sortBy poOrdering ps) in
    filter (\p -> length p == length mini) ps

poOrdering :: Pos -> Pos -> Ordering
poOrdering pos1 pos2
  | above   pos1 pos2 = LT
  | below   pos1 pos2 = GT
  | otherwise = EQ

-- parallel innermost
piStrategy :: Strategy
piStrategy = (\prog term -> getMinPosis (reduciblePos prog term))
 where
  getMinPosis :: [Pos] -> [Pos]
  getMinPosis [] = []
  getMinPosis ps = let mini = head (List.sortBy piOrdering ps) in
    filter (\p -> length p == length mini) ps

piOrdering :: Pos -> Pos -> Ordering
piOrdering pos1 pos2
  | below   pos1 pos2 = LT
  | above   pos1 pos2 = GT
  | otherwise = EQ

-- Reduces a term using a program at the first position provided by a given strategy
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog term
  | null stratPlan = Nothing 
  | otherwise = foldr (\pos acc -> if notNothing acc 
      then reduceAt prog (unwrap acc) pos 
      else reduceAt prog term pos
    )
    Nothing stratPlan
   where
    stratPlan :: [Pos]
    stratPlan = strat prog term


-- Evaluates a Term with a given Program until it is in its normal form.
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term
  | isNormalForm prog term = term
  | otherwise = evaluateWith strat prog (unwrap (reduceWith strat prog term))


-- -- Tests
-- addRules = Prog [Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"),
--   Rule (Comb "add" [Comb "SUCC" [Var "n"], Var "m"]) (Comb "SUCC" [Comb "add" [Var "n", Var "m"]])]

-- dumbRules = Prog [Rule (Var "x") (Var "x")]

-- term  = Var "m"
-- term1 = Comb "add" [Comb "ZERO" [], Comb "Zero" []]
-- term2 = Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]]
-- term3 = Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]]]

-- stupidRule = Prog [(Rule (Comb "add" [(Var "n"), (Var "m")]) (Comb "plus" [(Var "n"), (Var "m")])), (Rule (Comb "plus" [(Var "n"), (Var "m")]) (Comb "plüs" [(Var "n"), (Var "m")]))]

-- stupidTerm = Comb "add" [Comb "ZERO" [], Comb "ZERO" []]


addProg = Prog [(Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m")),
               (Rule (Comb "add" [Comb "SUCC" [Var "n"], Var "m"]) (Comb "SUCC" [Comb "add" [Var "n", Var "m"]]))]
testV4 = Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]]

test = evaluateWith loStrategy (Prog [Rule (Comb "add" [Comb "Zero" [],Var "m"]) (Var "m"),Rule (Comb "add" [Comb "Succ" [Var "n"],Var "m"]) (Comb "Succ" [Comb "add" [Var "n",Var "m"]])])
  (Comb "add" [Comb "Succ" [Comb "Succ" [],Comb "Zero" []],Comb "Succ" [Comb "Succ" [],Comb "Zero" []]])

test2 = evaluateWith loStrategy (Prog [Rule (Comb "add" [Comb "Zero" [],Var "m"]) (Var "m"),Rule (Comb "add" [Comb "Succ" [Var "n"],Var "m"]) (Comb "Succ" [Comb "add" [Var "n",Var "m"]]),Rule (Comb "mul" [Comb "Zero" [],Var "m"]) (Comb "Zero" []),Rule (Comb "mul" [Comb "Succ" [Var "n"],Var "m"]) (Comb "add" [Comb "mul" [Var "n",Var "m"],Var "m"]),Rule (Comb "double"
  [Var "x"]) (Comb "add" [Var "x",Var "x"]),Rule (Comb "square" [Var "x"]) (Comb "mul" [Var "x",Var "x"])])
  (Comb "add" [Comb "Succ" [Comb "Succ" [],Comb "Zero" []],Comb "Succ" [Comb "Succ" [],Comb "Zero" []]])