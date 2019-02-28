import Parser

-- main :: IO ()

getFile :: Parse a => String -> IO (Either String a)
getFile url = parseFile url

