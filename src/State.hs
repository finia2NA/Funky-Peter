module State where

import Evaluation
import Prog

type Path = String
type Name = String
type Progcluster = (Name, Prog, Maybe Path)

data State = State Strategy Progcluster

initalState :: State
initalState = State Evaluation.loStrategy ("", Prog [], Nothing)

setStrategy :: State -> Strategy -> State
setStrategy (State _ prog) strategy = State strategy prog

getStrategy :: State -> Strategy
getStrategy (State strat _) = strat

setProgram :: State -> Prog -> Name -> Path -> State
setProgram (State strat _) prog name path = State strat (name, prog, Just path)

getProgram :: State -> Prog
getProgram (State _ (_, prog, _)) = prog

getName :: State -> Name
getName (State _ (name, _, _)) = name

getPath :: State -> Maybe Path
getPath (State _ (_, _, path)) = path