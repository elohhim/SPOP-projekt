module Main where
 
import System.Environment

import Riddle
import Rendering
import Board
import Field

-- MAIN
main :: IO()
main = do
  args <- getArgs
  definition <- loadDefinition args
  print "Riddle definition loaded from file:"
  print definition
  let riddle = makeRiddle $ parseDefinition definition
  print "Riddle to solve:" 
  doRender $ riddle
  print "Riddle solution:"
  doRender $ solve riddle

loadDefinition :: [String] -> IO String
loadDefinition [] =  getPath >>= readFile where
  getPath = do
    putStrLn "Provide path to architect's riddle definition file:"
    getLine
loadDefinition (a:_) = readFile a

parseDefinition :: String -> ([Int], [Int], [(Int,Int)])
parseDefinition d = parseDefinition' $ lines d where
  parseDefinition' (rline:cline:hline:_) =
    (read rline, read cline, read hline)




