module Evaluation where

import Reduction
import Pos
import Term
import Prog
import Data.List

{-
WIKI:
innermost:
  Die Argumente eines Funktionsaufsrufs werden
  VOR der Funktionsanwendung ausgewertet.
  Wie in imperativen Sprachen: f(g(x)) -> g wird vor f ausgeführt

outermost:
  Funktionen werden von ausse
  n nach innen ausgeführt
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
    | above   pos1 pos2 = LT
    | below   pos1 pos2 = GT
    | leftOf  pos1 pos2 = LT
    | rightOf pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- left-innermost
liStrategy :: Strategy
liStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | below   pos1 pos2 = LT
    | above   pos1 pos2 = GT
    | leftOf  pos1 pos2 = LT
    | rightOf pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- right-outermost
roStrategy :: Strategy
roStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | above   pos1 pos2 = LT
    | below   pos1 pos2 = GT
    | rightOf pos1 pos2 = LT
    | leftOf  pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- right-innermost
riStrategy :: Strategy
riStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering
  strategy pos1 pos2
    | below   pos1 pos2 = LT
    | above   pos1 pos2 = GT
    | rightOf pos1 pos2 = LT
    | leftOf  pos1 pos2 = GT
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


-- reduceWith :: Strategy -> Prog -> Term -> Maybe Term
-- reduceWith str prg trm =

-- evaluateWith :: Strategy -> Prog -> Term -> Term
-- evaluateWith str prg trm =yya