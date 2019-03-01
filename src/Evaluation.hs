module Evaluation where

import Reduction
import Pos
import Term
import Prog
import Util
import Data.List

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
loStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | leftOf  pos1 pos2 = LT
    | rightOf pos1 pos2 = GT
    | above   pos1 pos2 = LT
    | below   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- left-innermost
liStrategy :: Strategy
liStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | leftOf  pos1 pos2 = LT
    | rightOf pos1 pos2 = GT
    | below   pos1 pos2 = LT
    | above   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- right-outermost
roStrategy :: Strategy
roStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | rightOf pos1 pos2 = LT
    | leftOf  pos1 pos2 = GT
    | above   pos1 pos2 = LT
    | below   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- right-innermost
riStrategy :: Strategy
riStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | rightOf pos1 pos2 = LT
    | leftOf  pos1 pos2 = GT
    | below   pos1 pos2 = LT
    | above   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"


-- -- parallel outermost
-- poStrategy :: Strategy
-- poStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
--  where
--   strategy :: Pos -> Pos -> Ordering

-- -- parallel innermost
-- piStrategy :: Strategy
-- piStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
--  where
--   strategy :: Pos -> Pos -> Ordering

-- Reduces a term using a program at the first position provided by a given strategy
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog term
  | length poss < 1 = Nothing 
  | otherwise       = reduceAt prog term (poss !! 0)
   where poss = strat prog term

-- Evaluates a Term with a given Program until it is in its normal form.
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term
  | isNormalForm prog term = term
  | otherwise = evaluateWith strat prog (unwrap (reduceWith strat prog term))


-- Tests
addRules = Prog [Rule (Comb "add" [Comb "ZERO" [], Var "m"]) (Var "m"),
  Rule (Comb "add" [Comb "SUCC" [Var "n"], Var "m"]) (Comb "SUCC" [Comb "add" [Var "n", Var "m"]])]

term1 = Comb "add" [Comb "ZERO" [], Comb "Zero" []]
term2 = Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]]
term3 = Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "add" [Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]], Comb "SUCC" [Comb "SUCC" [Comb "ZERO" []]]]]