module Row 
( Row(..)
) where

import qualified Data.Map as Map

import Field
import Rendering

data Row = Row { number :: Int
               , size :: Int
               , fields :: Map.Map (Int, Int) Field
               } deriving (Show)

instance Renderable Row
  where
    render row = concat $ map (maybe "\x25A1" render) (asList row)
    
instance FieldContainer Row
  where
    getFields = fields
    
asList :: Row -> [Maybe Field]
asList (Row n len fields) = [Map.lookup (n, col) fields | col <- [0..len-1]]


