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

-- left vs right vs parallel:
overview:
  in when given multiple reducible positions among a path within a tree,
  you have to specify whether to reduce the left/right side first
  and recalculate the reducible positions or reduce both sides and then
  recalculate.

parallel:
  With parallel evaluation, all reducible terms on one layer are evaluated
  in a single step before calculating the next list of reducible positions.
  

-- innermost vs outermost:

innermost:
  The arguments of the function call
  are evaluated before applying function itself.
  For example, in f(g(x)), g gets evaluated before f.


outermost:
  Whenever possible, outer function calls are evaluated first.
  This way of evaluating functions is more powerful than innermost evaluation,
  since it will, for example, find a solution for the call 
    "f h"
  with the program
    "f x = 1 | h = h"
  , whereas innermost evaluation will not.  


-}

-- | Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

-- | left-outermost
loStrategy :: Strategy
loStrategy = (\prog term -> if null (Reduction.reduciblePos prog term) 
    then [] 
    else (head (List.sortBy loOrdering (reduciblePos prog term))) : []
  )
 where
  loOrdering :: Pos -> Pos -> Ordering
  loOrdering pos1 pos2
    | leftOf  pos1 pos2 = LT
    | rightOf pos1 pos2 = GT
    | above   pos1 pos2 = LT
    | below   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- | left-innermost
liStrategy :: Strategy
liStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy liOrdering (reduciblePos prog term))) : []
  )
 where
  liOrdering :: Pos -> Pos -> Ordering
  liOrdering pos1 pos2
    | leftOf  pos1 pos2 = LT
    | rightOf pos1 pos2 = GT
    | below   pos1 pos2 = LT
    | above   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- | right-outermost
roStrategy :: Strategy
roStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy roOrdering (reduciblePos prog term))) : []
  )
 where
  roOrdering :: Pos -> Pos -> Ordering
  roOrdering pos1 pos2
    | rightOf pos1 pos2 = LT
    | leftOf  pos1 pos2 = GT
    | above   pos1 pos2 = LT
    | below   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"

-- | right-innermost
riStrategy :: Strategy
riStrategy = (\prog term -> if null (reduciblePos prog term) 
    then [] 
    else (head (List.sortBy riOrdering (reduciblePos prog term))) : []
  )
 where
  riOrdering :: Pos -> Pos -> Ordering
  riOrdering pos1 pos2
    | rightOf pos1 pos2 = LT
    | leftOf  pos1 pos2 = GT
    | below   pos1 pos2 = LT
    | above   pos1 pos2 = GT
    | otherwise = error "error found by strategy ™"


-- | parallel outermost
poStrategy :: Strategy
poStrategy = (\prog term -> getMinPosis (reduciblePos prog term))
 where
  getMinPosis :: [Pos] -> [Pos]
  getMinPosis [] = []
  getMinPosis ps = let mini = head (List.sortBy poOrdering ps) in
    filter (\p -> length p == length mini) ps
   where
    poOrdering :: Pos -> Pos -> Ordering
    poOrdering pos1 pos2
      | above   pos1 pos2 = LT
      | below   pos1 pos2 = GT
      | otherwise = EQ

-- | parallel innermost
piStrategy :: Strategy
piStrategy = (\prog term -> getMinPosis (reduciblePos prog term))
 where
  getMinPosis :: [Pos] -> [Pos]
  getMinPosis [] = []
  getMinPosis ps = let mini = head (List.sortBy piOrdering ps) in
    filter (\p -> length p == length mini) ps
   where
    piOrdering :: Pos -> Pos -> Ordering
    piOrdering pos1 pos2
      | below   pos1 pos2 = LT
      | above   pos1 pos2 = GT
      | otherwise = EQ

-- | Reduces a term using a program at the first position
--   provided by a given strategy
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


-- | Evaluates a Term with a given Program until it is in its normal form.
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term
  | isNormalForm prog term = term
  | otherwise = evaluateWith strat prog (unwrap (reduceWith strat prog term))