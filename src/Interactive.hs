module Interactive(main) where
import System.FilePath.Posix

import Evaluation
import Parser
import Pretty
import Prog
import State
import Util

-- | This gets called when the compiled Program starts,
--   displays welcome message,
--   starts the shell-loop,
--   and says goodbye at the end.
main :: IO ()
main = do
  putPeter
  putStrLn "Type \":h(elp)\" for help."
  shell (Just State.initalState)
  putStrLn "Leaving. Bye. Have a nice day."
  return ()

-- | the user-interaction loop. For every time the user enters something, 
--   his/her input gets evaluated and a new recursion is startet.
--   if needed, the state is adjusted for the next recursion.
--   an argument of nothing signals the user called :quit in the last
--   recursion and we terminate.
shell :: Maybe State -> IO ()
shell Nothing      = return ()
shell (Just state) = do
  input <- getUserInput (getName state)
  nextState <- handleInput state input
  shell nextState

-- | gets the userinput as a string from the console.
getUserInput :: Name -> IO (String)
getUserInput progName = do
  putStr (progName ++ "> ")
  getLine

-- | handles userinput: determines wether the user entered an expression
--   or a command and proceeds accordingly.
handleInput :: State -> String -> IO (Maybe State)
handleInput state []    = return (Just state)
handleInput state input =
  case input of
    (':':_) -> parseAndEvalCommand state (tail input)
    _       -> parseAndEvalExpression state input

-- | interprets a given String as an expression and runs using the eval strat
--   encoded in the state.
parseAndEvalExpression :: State -> String -> IO (Maybe State)
parseAndEvalExpression state expr = do
  let eitherTerm = Parser.parse expr
  case eitherTerm of
    (Left msg) -> putStrLn msg >> return (Just state)
    (Right term) -> do
      let res = (Evaluation.evaluateWith (State.getStrategy state) (State.getProgram state) term)
      putStrLn (pretty res)
      return (Just state)

-- | interprets a given string as a command and returns a new state that
--   results from applying the command.
parseAndEvalCommand :: State -> String -> IO (Maybe State)
parseAndEvalCommand state [] = 
  putStrLn "Empty command! Enter \":h\" for a list of available commands." >> 
  return (Just state)
parseAndEvalCommand state command
  | head command == 'q' = return Nothing
  | head command == 'h' = printHelp >> return (Just state)
  | head command == 's' = updateStrategy state command
  | head command == 'l' = loadFile state command
  | head command == 'r' = reloadFile state
  | head command == 'e' = loadFile state "l ../examples/Examples.hs"
  | otherwise = 
      putStrLn "Unknown command! Enter \":h\" for a list of available commands."
       >> return (Just state)

-- | changes the evalStrategy encoded in the state.
updateStrategy :: State -> String -> IO (Maybe State)
updateStrategy state cmd = do
-- words is predefined Prelude func. Splits a string at the whitespaces into a list
  let args = words cmd
  if length args <= 1
    then putStrLn ("No strategy specified. " ++
      "Enter \":h\" for a list of available strategies.") >> return (Just state)
    else do
      let maybeStrat = parseStrategy (args !! 1)
      if Util.notNothing maybeStrat
        then putStrLn ("Strategy updated to: " ++ (args !! 1)) >> 
          return (Just (State.setStrategy state (Util.unwrap maybeStrat)))
        else putStrLn ("Unkown strategy. " ++
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

-- | replaces the loaded program, either with a new one or with an empty
--   program when not given an argument.
loadFile :: State -> String -> IO (Maybe State)
loadFile state cmd = do
  let args = words cmd
  if length args <= 1
    -- No path provided. We should unload the file if one is loaded.
    then if Util.notNothing (State.getPath state)
      then putStrLn ("File unloaded.") >>
        return (Just (State.setProgram state "" (Prog []) Nothing))
      else putStrLn ("No file loaded!") >> return (Just state)
    else do
      -- Path provided. Let's load that file.
      let filePath = args !! 1
      eitherProg <- Parser.parseFile filePath
      case eitherProg of
        (Left msg) -> putStrLn msg >> 
          return (Just (State.setProgram state "" (Prog []) (Just filePath)))
        (Right prog) -> putStrLn ("File loaded.") >>
          return (Just (State.setProgram state 
            (System.FilePath.Posix.takeBaseName filePath) prog (Just filePath)))

-- | loads the program at the path described in the state.
reloadFile :: State -> IO (Maybe State)
reloadFile state = let path = (State.getPath state) in
  if Util.notNothing path
    then loadFile state ("l " ++ (Util.unwrap path))
    else putStrLn ("Reloading something that does not exists is pretty much useless.") 
      >> return (Just state)

-- | prints the help information.
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

-- | displays an image of our glorious mascot, funky peter!
putPeter :: IO ()
putPeter = do
  putStrLn "             .,,,,,/                   " 
  putStrLn "           ,.**,*,*/((*                "
  putStrLn "          ,*/,,,..,,/(*                "
  putStrLn "          ,***,,,..,*(/*               "
  putStrLn "          .,,*,..,*///(/               "
  putStrLn "          ,,**/,/,*//,*/               "
  putStrLn " Welcome   .**,.**/##//#        to     "
  putStrLn "           ,***/,/*..(/(               "
  putStrLn " Funky        **,,,**(/#%       Peter! "
  putStrLn "              */**.,,/#/%&&&&          "
  putStrLn "           %%%,.*,*/,*/%%%%%%&&&&&     "
  putStrLn "       #%%%%%%%.((#&%*%%%%%%%%%%%&&&&  "
  putStrLn "     %%%%%%%%%%*,&,%%%%%%%&%%%%%%&&&   "
  putStrLn "   %&%%%%%%%%%%./%(//%%%%%%&%%%%&%%%%( "
  putStrLn "   &%%%%%%%%%%%,%(#%%%%%%%%%%&%%%&%%%##"
  putStrLn "   &%%%&&%%%%%%(/%(#%%%%%&%%&&%%%%%&&&%"
  putStrLn "   %%%%%%%%%%%%%%//&%%%%&%%&&%%%%&&&%&%"
  putStrLn "  %%%%%%&%%%%%%(*#/&%%%&%%&&&%%%%%%%%&&"
  putStrLn " .&./%,*&&&&&&&(%&&&@&&&&&&%%%%%%&&&%  "
  putStrLn " %(.,*,//*(&&&@@%%#%&%#(//**(%%%%%%&&&&"
  putStrLn ""