module Main where
 
import System.Environment

import Riddle

-- MAIN
main :: IO()
main = do
  args <- getArgs
  definition <- loadDefinition args
  putStrLn "Riddle definition loaded from file:"
  putStrLn definition
  let riddle = makeRiddle $ parseDefinition definition
  putStr $ render (solveRiddle riddle)
 

loadDefinition :: [String] -> IO String
loadDefinition [] =  getPath >>= readFile where
  getPath = do
    putStrLn "Provide path to architect's riddle definition file:"
    getLine
loadDefinition (a:_) = readFile a

parseDefinition :: String -> (RowDef, ColDef, HouseDef)
parseDefinition d = parseDefinition' $ lines d where
  parseDefinition' (rline:cline:hline:_) =
    (read rline, read cline, read hline)




