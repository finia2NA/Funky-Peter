import Parser
import State
import Evaluation
import Pretty
import Util

-- main :: IO ()

-- data State s p= State (Strategy s) (Program p)

-- programmLoop :: IO State -> IO State
-- getFile :: Parse a => String -> IO (Either String a)
-- getFile url = parseFile url

main :: IO ()
main = do
  printWelcome
  shell State.initalState
  return ()

shell :: State -> IO ()
shell state = do
  input <- getUserInput (getName state)
  nextState <- parseAndEvalInput state input
  if notNothing nextState
  then shell (unwrap nextState)
  else return ()

printWelcome :: IO ()
printWelcome = do
  print "Welcome to Simple Haskell!"
  print "Type \":h(elp)\" for help."

getUserInput :: Name -> IO (String)
getUserInput progName = do
  putStr (progName ++ "> ")
  getLine

parseAndEvalInput :: State -> String -> IO (Maybe State)
parseAndEvalInput state input =
  case input of
    (':':_) -> 
      return (Nothing)
      -- return (shelp state (tail input))
    _ -> do
      let eitherTerm = Parser.parse input in
        case eitherTerm of
          (Left msg) -> print msg >> return (Just state)
          (Right term) -> do
            let res = Evaluation.evaluateWith (State.getStrategy state) (State.getProgram state) term
            print (pretty res)
            return (Just state)

-- Use next level advanced AI to parse the users will
-- shelp :: State -> String -> IO (Maybe State)
-- shelp oldState command
--   | head command == 'r'= Just (setProgram state )
--   | head command == 'h'=
--     helphelper
--   | head command == 'l'=

--   | head command == 's'=

--   | head command == 'q' =

-- shelp oldState ('r':xs) =

-- shelp