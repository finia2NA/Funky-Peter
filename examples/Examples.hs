module Example where
  -- 8.1, 5
  -- Regeln für 8.1, 5:
  f :: Peano -> Peano
  f x = Zero
  
  h :: Peano
  h = h
  
-- | 8.1:
--   wenn man (f h) in einem call-by-value modus ausführt, bekommt man eine
--   unendliche rekursion. bei call-by-name eval bekommt man das ergebniss Zero.
exp1 = f h

-- | 8.5:
--   da die funktion h aus 8.1 sich konstant selbst aufruft und nice tatsächlich
--   eine peano-zahl returnt, terminiert sie nie.
exp5 = h



-- 8.2, 3

-- daten für 8.2, 3
data Boolean = True | False


-- Regeln für 8.2, 3
f2 _ True = True
f2 _ False = False

f3 True _ = True
f3 False _ = False

genTrue = True

endlessBool :: Bool
endlessBool = endlessBool


-- | Lösung für 8.2: Terminiert für RO, nicht für LO
exp2 = f2 endlessBool genTrue

-- | Lösung für 8.3: Terminiert für LO, nicht für RO
exp3 = f3 genTrue endlessBool
