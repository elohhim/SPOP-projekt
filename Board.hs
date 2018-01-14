-- Provides data type and methods for manipulating whole riddle board
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

-- Data type representing riddle board.
data Board = Board { size :: (Int, Int)
                   , fields :: FieldMap
                   } deriving (Show)

instance Renderable Board
  where
    render board = unlines (map render (asRows board))
    
instance FieldContainer Board
  where
    getFields = fields
        
-- Factory method which allow creating Board.
makeBoard :: Int -> Int -> FieldAssociationList -> Board
makeBoard rows cols fields = Board (rows, cols) (M.fromList fields)

-- Methods operating on board use rows. When we need to use column it is easier
-- to transpose board and call method on row corresponding to column
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

-- Set board field of given coordinates.
setField :: (Int, Int) -> Field -> Board -> Board
setField coord field (Board size fieldMap) = Board size newFields
  where
    newFields = M.insertWith (\new old -> old) coord field fieldMap

-- Set multiple board fields at once. Fields are only set if empty as union is
-- favoring left side. 
setFields :: [(Int, Int)] -> Field -> Board -> Board
setFields coords field (Board size fieldMap) = Board size newFields
  where
    newFields = M.union fieldMap (M.fromList [(coord, field) | coord <- coords])

-- Returns coordinates list of fields that are adjacent to field at given
-- coordinate. If first argument is True it returns all 8 neighbors otherwise
-- only 4 which share edge with queried one. 
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

-- Return coordinates of empty fields adjacent to field of given coordintes.
adjacentCoordEmpty :: (Int, Int) -> Board -> [(Int, Int)]
adjacentCoordEmpty coord board = filterEmpty (adjacentCoord coord board)
  where
    filterEmpty =  filter (`fieldEmpty` board)

-- Sets field of given coordinates to Tank and then cross-out (sets to Grass)
-- all empty fields which surround it.
buildTank :: (Int, Int) -> Board -> Board
buildTank coord board = surroundTank . placeTank $ board
  where
    placeTank b = setField coord Tank b
    surroundTank b = setFields (adjacentCoordFull coord b) Grass b