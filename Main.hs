module Main where
 
import System.Environment

import Riddle
import Rendering

-- MAIN - entry point for application
main :: IO()
main = do
  args <- getArgs
  definition <- loadDefinition args
  putStrLn "Riddle definition loaded from file:"
  putStrLn definition
  let riddle = makeRiddle $ parseDefinition definition
  putStrLn "Riddle to solve:" 
  doRender $ riddle
  putStrLn "Riddle solution:"
  doRender $ solve riddle

-- Prompt user for path and read file containing riddle definition.
loadDefinition :: [String] -> IO String
loadDefinition [] =  getPath >>= readFile where
  getPath = do
    putStrLn "Provide path to architect's riddle definition file:"
    getLine
loadDefinition (a:_) = readFile a

-- Parse content of file with riddle definition. Returns data in form compatible
-- with makeRiddle method.
parseDefinition :: String -> ([Int], [Int], [(Int,Int)])
parseDefinition d = parseDefinition' $ lines d where
  parseDefinition' (rline:cline:hline:_) =
    (read rline, read cline, read hline)




