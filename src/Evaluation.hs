module Evaluation where

import Reduction
import Pos
import Term
import Prog

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

-- left-innermost
liStrategy :: Strategy
liStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering

-- right-outermost
roStrategy :: Strategy
roStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering

-- right-innermost
riStrategy :: Strategy
riStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering

-- parallel outermost
poStrategy :: Strategy
poStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering

-- parallel innermost
piStrategy :: Strategy
piStrategy = (\prog term -> sortBy strategy (reduciblePos prog term))
 where
  strategy :: Pos -> Pos -> Ordering


-- reduceWith :: Strategy -> Prog -> Term -> Maybe Term
-- reduceWith str prg trm =

-- evaluateWith :: Strategy -> Prog -> Term -> Term
-- evaluateWith str prg trm =yya