import System.FilePath.Posix
import Parser
import State
import Evaluation
import Pretty
import Util
import Prog
import Term

main :: IO ()
main = do
  putStrLn "Welcome to Funky Peter!"
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
handleInput state []    = return (Just state)
handleInput state input =
  case input of
    (':':_) -> parseCommand state (tail input)
    _       -> parseExpression state input

parseExpression :: State -> String -> IO (Maybe State)
parseExpression state expr = do
  putStrLn "expression ="
  putStrLn expr
  let eitherTerm = Parser.parse expr
  case eitherTerm of
    (Left msg) -> putStrLn msg >> return (Just state)
    (Right term) -> do
      putStrLn "term = "
      putStrLn (show term)
      putStrLn "program ="
      putStrLn (show (State.getProgram state))
      putStrLn "\n"
      let res = (Evaluation.evaluateWith (State.getStrategy state) (State.getProgram state) term)
      putStrLn (pretty res)
      return (Just state)

-- Use next level advanced AI to parse the users will
parseCommand :: State -> String -> IO (Maybe State)
parseCommand state [] = putStrLn "Empty command! Enter \":h\" for a list of available commands." >> return (Just state)
parseCommand state command
  | head command == 'q' = return Nothing
  | head command == 'h' = printHelp >> return (Just state)
  | head command == 's' = updateStrategy state command
  | head command == 'l' = loadFile state command
  | head command == 'r' = reloadFile state
  | otherwise = putStrLn "Unknown command! Enter \":h\" for a list of available commands." >> return (Just state)

updateStrategy :: State -> String -> IO (Maybe State)
updateStrategy state cmd = do
-- words is predefined Prelude func. Splits a string at the whitespaces into a list
  let args = words cmd
  if length args <= 1
    then putStrLn ("No strategy specified." ++
      "Enter \":h\" for a list of available strategies.") >> return (Just state)
    else do
      let maybeStrat = parseStrategy (args !! 1)
      if Util.notNothing maybeStrat
        then putStrLn ("Strategy updated to: " ++ (args !! 1)) >> 
          return (Just (State.setStrategy state (Util.unwrap maybeStrat)))
        else putStrLn ("Unkown strategy." ++
        "Enter \":h\" for a list of available strategies.") >> return (Just state)
 where
  parseStrategy :: String -> Maybe Strategy
  parseStrategy strat 
    | strat == "lo" = Just Evaluation.loStrategy
    | strat == "li" = Just Evaluation.liStrategy
    | strat == "ro" = Just Evaluation.roStrategy
    | strat == "ri" = Just Evaluation.riStrategy
    | strat == "po" = Just Evaluation.poStrategy
    | strat == "pi" = Just Evaluation.piStrategy
    | otherwise     = Nothing

loadFile :: State -> String -> IO (Maybe State)
loadFile state cmd = do
  let args = words cmd
  if length args <= 1
    -- No path provided. We should unlaod the file if one is loaded.
    then if Util.notNothing (State.getPath state)
      then putStrLn ("File unloaded.") >>
        return (Just (State.setProgram state "" (Prog []) Nothing))
      else putStrLn ("No file loaded!") >> return (Just state)
    else do
      -- Path provided. Let's load that file.
      let filePath = args !! 1
      eitherProg <- Parser.parseFile filePath
      case eitherProg of
        (Left msg) -> putStrLn msg >> return (Just state)
        (Right prog) -> putStrLn ("File loaded.") >>
          return (Just (State.setProgram state 
            (System.FilePath.Posix.takeBaseName filePath) prog (Just filePath)))

reloadFile :: State -> IO (Maybe State)
reloadFile state = let path = (State.getPath state) in
  if Util.notNothing path
    then loadFile state ("l " ++ (Util.unwrap path))
    else putStrLn ("Reloading something that does not exists is pretty much useless.") 
      >> return (Just state)

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


test = evaluateWith loStrategy (Prog [Rule (Comb "add" [Comb "Zero" [],Var "m"]) (Var "m"),Rule (Comb "add" [Comb "Succ" [Var "n"],Var "m"]) (Comb "Succ" [Comb "add" [Var "n",Var "m"]]),Rule (Comb "mul" [Comb "Zero" [],Var "m"]) (Comb "Zero" []),Rule (Comb "mul" [Comb "Succ" [Var "n"],Var "m"]) (Comb "add" [Comb "mul" [Var "n",Var "m"],Var "m"]),Rule (Comb "double"
  [Var "x"]) (Comb "add" [Var "x",Var "x"]),Rule (Comb "square" [Var "x"]) (Comb "mul" [Var "x",Var "x"])])
  (Comb "add" [Comb "Succ" [Comb "Succ" [],Comb "Zero" []],Comb "Succ" [Comb "Succ" [],Comb "Zero" []]])