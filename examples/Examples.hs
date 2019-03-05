module Example where
  -- 8.1, 5
  f :: Peano -> Peano
  f x = Zero
  
  h :: Peano
  h = h
  
  -- wenn man (f h) in einem call-by-value modus ausführt, bekommt man eine
  -- unendliche rekursion. bei call-by-name eval bekommt man das ergebniss Zero.
exp1 = f h

-- da die funktion h aus 8.1 sich konstant selbst aufruft und nice tatsächelich
-- eine peano-zahl returnt, terminiert sie nie.
exp5 = h

-- 8.2, 3
data Boolean = True | False

or :: Boolean -> Boolean -> Boolean
or False x = isTrue x
or True _ = True

and :: Boolean -> Boolean -> Boolean
and True x = isTrue x
and False _ = False

isTrue :: Boolean -> Boolean
isTrue True = True
isTrue False = False

endlessBool :: Bool
endlessBool = endlessBool

exp3 = or True endlessBool
