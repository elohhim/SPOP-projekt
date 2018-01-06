module Board 
( Board (..)
, makeBoard
, transposeBoard
, row
, asRows
, fromRows) where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

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
makeBoard rows cols fields = Board (rows, cols) (Map.fromList fields)

transposeBoard :: Board -> Board
transposeBoard (Board size fields) = Board (swap size) (transposeMap fields)

row :: Int -> Board -> R.Row
row n (Board (_, cols) fieldMap) = R.Row n cols rowFields
  where
    rowFields = Map.filterWithKey (\k _ -> (fst k) == n) fieldMap

asRows :: Board -> [R.Row]
asRows board = [row n board | n <- [0..rowCount-1] ]
  where
    rowCount = fst $ size board

fromRows :: [R.Row] -> Board
fromRows rs = Board (length rs, R.size $ head rs) boardFields
  where
    boardFields = Map.unions [getFields r | r <- rs]