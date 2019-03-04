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
  putStrLn "Welcome to Simple Haskell!"
  putStrLn "Type \":h(elp)\" for help."
  shell (Just State.initalState)
  putStrLn "Leaving. Bye. Have a nice day."
  return ()

shell :: Maybe State -> IO ()
shell Nothing      = return ()
shell (Just state) = do
  input <- getUserInput (getName state)
  nextState <- handleInput state input
  shell nextState

getUserInput :: Name -> IO (String)
getUserInput progName = do
  putStr (progName ++ "> ")
  getLine

handleInput :: State -> String -> IO (Maybe State)
handleInput state input =
  case input of
    (':':_) -> parseCommand state (tail input)
    _       -> parseExpression state input

parseExpression :: State -> String -> IO (Maybe State)
parseExpression state expr = do
  let eitherTerm = Parser.parse expr
  case eitherTerm of
    (Left msg) -> putStrLn msg >> return (Just state)
    (Right term) -> do
      let res = Evaluation.evaluateWith (State.getStrategy state) (State.getProgram state) term
      putStrLn (pretty res)
      return (Just state)

-- Use next level advanced AI to parse the users will
parseCommand :: State -> String -> IO (Maybe State)
parseCommand state [] = putStrLn "Empty command! Enter \":h\" for a list of available commands." >> return (Just state)
parseCommand state command
  | head command == 'q' = return Nothing
  | head command == 'h' = printHelp >> return (Just state)
  | head command == 's' = updateStrategy state command
  -- | head command == 'r' = Just (setProgram state )
  -- | head command == 'l'=
  | otherwise = putStrLn "Unknown command! Enter \":h\" for a list of available commands." >> return (Just state)


updateStrategy :: State -> String -> IO (Maybe State)
updateStrategy state cmd = do
  let args = words cmd
  if length args <= 1
    then putStrLn ("No strategy specified." ++
      "Enter \":h\" for a list of available strategies.") >> return (Just state)
    else do
      let maybeStrat = parseStrategy (args !! 1)
      if notNothing maybeStrat
        then putStrLn ("Strategy updated to: " ++ (args !! 1)) >> 
          return (Just (setStrategy state (unwrap maybeStrat)))
        else putStrLn ("Unkown strategy." ++
        "Enter \":h\" for a list of available strategies.") >> return (Just state)
 where
  parseStrategy :: String -> Maybe Strategy
  parseStrategy strat 
    | strat == "lo" = Just loStrategy
    | strat == "li" = Just liStrategy
    | strat == "ro" = Just roStrategy
    | strat == "ri" = Just riStrategy
    | strat == "po" = Just poStrategy
    | strat == "pi" = Just piStrategy
    | otherwise     = Nothing

printHelp :: IO()
printHelp = do
  putStrLn "Commands available from the prompt:"
  putStrLn "  <expression>       Evaluates the specified expression."
  putStrLn "  :h[elp]            Shows this help message."
  putStrLn "  :l[oad] <file>     Loads the specified file."
  putStrLn "  :l[oad]            Unloads the currently loaded file."
  putStrLn "  :r[eload]          Reloads the lastly loaded file."
  putStrLn "  :s[et] <strategy>  Sets the specified evaluation strategy"
  putStrLn "                     where <strategy> is one of 'lo', 'li',"
  putStrLn "                     'ro', 'ri', 'po', or 'pi'."
  putStrLn "  :q[uit]            Exits the interactive environment."