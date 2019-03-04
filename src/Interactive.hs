import Parser
import State
import Evaluation
import Pretty

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
  input <- getUserInput
  state <- parseAndEvalInput state input
  if notNothing state
  then shell state
  else return ()

printWelcome :: IO ()
printWelcome = do
  print "Welcome to Simple Haskell!"
  print "Type \":h(elp)\" for help."

getUserInput :: Name -> IO (String)
getUserInput progName = do
  putStr (progName ++ "> ")
  return getLine

parseAndEvalInput :: State -> String -> IO (Maybe State)
parseAndEvalInput state input =
  case input
    of (':':_) -> do 
      return (shelp state (tail input))
    of _  -> do
            res <- Evaluation.evaluateWith
                    (State.getStrategy state)
                    (State.getProgram)
                    (Parser.parse input)
            print (pretty res)
            return (Just state)

-- Use next level advanced AI to parse the users will
shelp :: State -> String -> IO (Maybe State)
shelp oldState command
  | head command == 'r'= Just (setProgram state )
  | head command == 'h'=
    helphelper
  | head command == 'l'=

  | head command == 's'=

  | head command == 'q' =

shelp oldState ('r':xs) =

shelp