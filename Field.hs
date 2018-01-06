module Field 
( Field(..)
, FieldMap
, FieldAssociationList
, FieldContainer(..)
, transposeMap) where

import Rendering
import Data.Tuple (swap)
import qualified Data.Map as M

type FieldMap = M.Map (Int, Int) Field
type FieldAssociationList = [((Int, Int), Field)]

class FieldContainer a
  where
    getFields :: a -> FieldMap
    countFields :: Field -> a -> Int
    countFields f c = M.size $ M.filter (==f) (getFields c)


data Field = Grass | House | Tank deriving (Eq, Show)

instance Renderable Field
  -- need to run "chcp.com 65001" in cmd to enable showing unicode symbols
  where
    render Grass = "\x25A8" -- stiped square
    render House = "\x2302" -- unicode home
    render Tank = "\x26AB" -- black circle
    
transposeMap :: M.Map (Int, Int) a -> M.Map (Int, Int) a
transposeMap = M.mapKeys swap