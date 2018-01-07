module Board 
( Board (..)
, makeBoard
, transposeBoard
, row
, asRows
, fromRows
, setField
, setFields
, buildTank
, adjacentCoordEmpty) where

import qualified Data.Map as M
import Data.Tuple (swap)
import Data.List (sort)

import Rendering
import Field
import qualified Row as R

data Board = Board { size :: (Int, Int)
                   , fields :: FieldMap
                   } deriving (Show)

instance Renderable Board
  where
    render board = unlines (map render (asRows board))
    
instance FieldContainer Board
  where
    getFields = fields
        
makeBoard :: Int -> Int -> FieldAssociationList -> Board
makeBoard rows cols fields = Board (rows, cols) (M.fromList fields)

transposeBoard :: Board -> Board
transposeBoard (Board size fields) = Board (swap size) (transposeMap fields)

row :: Int -> Board -> R.Row
row n (Board (_, cols) fieldMap) = R.Row n cols rowFields
  where
    rowFields = M.filterWithKey (\k _ -> (fst k) == n) fieldMap

asRows :: Board -> [R.Row]
asRows board = [row n board | n <- [0..rowCount-1] ]
  where
    rowCount = fst $ size board

fromRows :: [R.Row] -> Board
fromRows rs = Board (length rs, R.size $ head rs) boardFields
  where
    boardFields = M.unions [getFields r | r <- rs]
    
setField :: (Int, Int) -> Field -> Board -> Board
setField coord field (Board size fieldMap) = Board size newFields
  where
    newFields = M.insertWith (\new old -> old) coord field fieldMap

setFields :: [(Int, Int)] -> Field -> Board -> Board
setFields coords field (Board size fieldMap) = Board size newFields
  where
    newFields = M.union fieldMap (M.fromList [(coord, field) | coord <- coords])

    
adjacentCoord' :: Bool -> (Int, Int) -> Board -> [(Int, Int)]
adjacentCoord' full (r, c) board = sort $ filter validCoord (coord full)
  where
    coord False = sides
    coord True = sides ++ corners
    sides = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
    corners = [(r - 1, c - 1), (r - 1, c + 1), (r + 1, c -1), (r + 1, c + 1)]
    validCoord (r', c') = and [r' >= 0, c' >= 0, r' < rows, c' < cols]
    rows = fst $ size board
    cols = snd $ size board

adjacentCoord :: (Int, Int) -> Board -> [(Int, Int)]
adjacentCoord = adjacentCoord' False    
    
adjacentCoordFull :: (Int, Int) -> Board -> [(Int, Int)]
adjacentCoordFull = adjacentCoord' True

adjacentCoordEmpty :: (Int, Int) -> Board -> [(Int, Int)]
adjacentCoordEmpty coord board = filterEmpty (adjacentCoord coord board)
  where
    filterEmpty =  filter (`fieldEmpty` board)
    
buildTank :: (Int, Int) -> Board -> Board
buildTank coord board = surroundTank . placeTank $ board
  where
    placeTank b = setField coord Tank b
    surroundTank b = setFields (adjacentCoordFull coord b) Grass b

