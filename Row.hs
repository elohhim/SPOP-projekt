-- Provides data type and methods for manipulating riddle board  as rows
module Row 
( Row(..)
, asList
, plantGrass) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Field
import Rendering

-- Data type representing row of board
data Row = Row { number :: Int
               , size :: Int
               , fields :: FieldMap
               } deriving (Show)

instance Renderable Row
  where
    render row = concat $ map (maybe char render) (asList row)
      where
        char = " "--"\x25A1"
    
instance FieldContainer Row
  where
    getFields = fields

-- Transform Row into explicit list of Maybe Field where Nothing means empty
-- field. This representation is more convenient in some cases.
asList :: Row -> [Maybe Field]
asList (Row n len fields) = [M.lookup (n, col) fields | col <- [0..len-1]]

-- Opposite to asList.
fromList :: Int -> [Maybe Field] -> Row
fromList n fs = Row n (length fs) fieldMap
  where
    fieldMap = M.fromList $ mapMaybe assocciation (zip [0..] fs)
    assocciation (c, Just f) = Just ((n,c), f)
    assocciation (_, Nothing) = Nothing

-- Set all empty fields to Grass.
plantGrass :: Row -> Row
plantGrass row = fromList (number row) (plantGrass' (asList row)) 
  where
    plantGrass' [] = []
    plantGrass' (f:fs) = if f == Nothing
                         then (Just Grass):(plantGrass' fs)
                         else f:(plantGrass' fs)