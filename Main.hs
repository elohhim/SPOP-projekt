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
  let parsedDef = parseDefinition definition
  if validateDefinition parsedDef
    then putStrLn "Definition is VALID."
    else error "Definition is INVALID!"
  let riddle = makeRiddle parsedDef 
  putStrLn "Riddle to solve:" 
  doRender $ riddle
  putStrLn "Provide version of algorithm to be used (1 or not 1):"
  algVersion <- getLine
  if algVersion == "1" 
    then putStrLn "Algorithm chosen: 1"
    else putStrLn "Algorithm chosen: 2"
  putStrLn "Riddle solution:"
  let solution = if algVersion == "1" then solve riddle else solve2 riddle
  doRender solution
  putStrLn "Provide path to dump solution:"
  dumpPath <- getLine
  writeFile dumpPath (render solution)
  

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