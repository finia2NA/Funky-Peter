import Parser

-- main :: IO ()

-- data State s p= State (Strategy s) (Program p)

-- programmLoop :: IO State -> IO State
-- getFile :: Parse a => String -> IO (Either String a)
-- getFile url = parseFile url

printWelcome :: IO ()
printWelcome = do
  print "Welcome to Simple Haskell!"
  print "Type \":h(elp)\" for help."

main :: IO ()
main = do
  printWelcome 