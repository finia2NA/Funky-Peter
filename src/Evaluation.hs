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

import qualified Data.List as List

import Pos
import Prog
import Reduction
import Term
import Util

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

-- | returns a list containing the next element to reduce accourding to the
--   left-outermost strategy
loStrategy :: Strategy
loStrategy = (\prog term -> if null (Reduction.reduciblePos prog term) 
    then [] 
    else (head (List.sortBy loOrdering (Reduction.reduciblePos prog term))) : []
  )
 where
  loOrdering :: Pos -> Pos -> Ordering
  loOrdering pos1 pos2
    | Pos.leftOf  pos1 pos2 = LT
    | Pos.rightOf pos1 pos2 = GT
    | Pos.above   pos1 pos2 = LT
    | Pos.below   pos1 pos2 = GT
    | otherwise = error "non-exhaustive pattern in strategy"

-- | returns a list containing the next element to reduce accourding to the
--   left-innermost strategy
liStrategy :: Strategy
liStrategy = (\prog term -> if null (Reduction.reduciblePos prog term) 
    then [] 
    else (head (List.sortBy liOrdering (Reduction.reduciblePos prog term))) : []
  )
 where
  liOrdering :: Pos -> Pos -> Ordering
  liOrdering pos1 pos2
    | Pos.leftOf  pos1 pos2 = LT
    | Pos.rightOf pos1 pos2 = GT
    | Pos.below   pos1 pos2 = LT
    | Pos.above   pos1 pos2 = GT
    | otherwise = error "non-exhaustive pattern in strategy"

-- | returns a list containing the next element to reduce accourding to the
--   right-outermost strategy
roStrategy :: Strategy
roStrategy = (\prog term -> if null (Reduction.reduciblePos prog term) 
    then [] 
    else (head (List.sortBy roOrdering (Reduction.reduciblePos prog term))) : []
  )
 where
  roOrdering :: Pos -> Pos -> Ordering
  roOrdering pos1 pos2
    | Pos.rightOf pos1 pos2 = LT
    | Pos.leftOf  pos1 pos2 = GT
    | Pos.above   pos1 pos2 = LT
    | Pos.below   pos1 pos2 = GT
    | otherwise = error "non-exhaustive pattern in strategy"

-- | returns a list containing the next element to reduce accourding to the
--   right-innermost strategy
riStrategy :: Strategy
riStrategy = (\prog term -> if null (Reduction.reduciblePos prog term) 
    then [] 
    else (head (List.sortBy riOrdering (Reduction.reduciblePos prog term))) : []
  )
 where
  riOrdering :: Pos -> Pos -> Ordering
  riOrdering pos1 pos2
    | Pos.rightOf pos1 pos2 = LT
    | Pos.leftOf  pos1 pos2 = GT
    | Pos.below   pos1 pos2 = LT
    | Pos.above   pos1 pos2 = GT
    | otherwise = error "non-exhaustive pattern in strategy"

-- | returns a list containing the next elements to reduce accourding to the
--   parallel outermost strategy
poStrategy :: Strategy
poStrategy = (\prog term -> getMinPosis (Reduction.reduciblePos prog term))
 where
  getMinPosis :: [Pos] -> [Pos]
  getMinPosis [] = []
  getMinPosis ps = let mini = head (List.sortBy poOrdering ps) in
    filter (\p -> length p == length mini) ps
   where
    poOrdering :: Pos -> Pos -> Ordering
    poOrdering pos1 pos2
      | Pos.above   pos1 pos2 = LT
      | Pos.below   pos1 pos2 = GT
      | otherwise = EQ

-- | returns a list containing the next elements to reduce accourding to the
--   parallel innermost strategy
piStrategy :: Strategy
piStrategy = (\prog term -> getMinPosis (Reduction.reduciblePos prog term))
 where
  getMinPosis :: [Pos] -> [Pos]
  getMinPosis [] = []
  getMinPosis ps = let mini = head (List.sortBy piOrdering ps) in
    filter (\p -> length p == length mini) ps
   where
    piOrdering :: Pos -> Pos -> Ordering
    piOrdering pos1 pos2
      | Pos.below   pos1 pos2 = LT
      | Pos.above   pos1 pos2 = GT
      | otherwise = EQ

-- | Reduces a term using a program at the first position
--   provided by a given strategy
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog term
  | null stratPlan = Nothing 
  | otherwise = foldr (\pos acc -> if Util.notNothing acc 
      then Reduction.reduceAt prog (unwrap acc) pos 
      else Reduction.reduceAt prog term pos
    )
    Nothing stratPlan
   where
    stratPlan :: [Pos]
    stratPlan = strat prog term


-- | Evaluates a Term with a given Program until it is in its normal form.
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term
  | isNormalForm prog term = term
  | otherwise = evaluateWith strat prog (
    Util.unwrap (reduceWith strat prog term)
  )