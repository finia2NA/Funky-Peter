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

genTrue = True

f3 True _ = True
f3 False _ = False

f2 _ True = True
f2 _ False = False

endlessBool :: Bool
endlessBool = endlessBool

exp2 = f2 endlessBool genTrue
exp3 = f3 genTrue endlessBool


-- 8.4
t :: Boolean -> Boolean
t True = True
t False = t False

comp :: Boolean -> Boolean -> Boolean
comp t1 x = (t1 x)

exp4 = comp t genTrue
