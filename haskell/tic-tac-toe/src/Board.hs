{-# LANGUAGE StandaloneDeriving #-}
module Board (Board(..), ixBoard, emptyBoard, Mark(..), modify) where

import Triple

newtype Board = Board (T (T Cell))

deriving instance Show Board
deriving instance Eq Board

data Mark = Xmark | Omark deriving (Show, Eq)

data Cell = X | O | E deriving (Show, Eq)
-- Note: E is empty

emptyBoard :: Board
emptyBoard = Board $ T (emptyRow, emptyRow, emptyRow) where
  emptyRow = T (E,E,E)

-- Given the (row, col) and mark, update that value
modify :: Board -> Mark -> (Tix, Tix) -> Board
modify (Board b) mark (row,col) = Board $ update b row newRow where
  newRow = update (b !: row) col (markToCell mark)

  markToCell :: Mark -> Cell
  markToCell Xmark = X
  markToCell Omark = O

ixBoard :: Board -> (Tix, Tix) -> Maybe Mark
ixBoard (Board b) (r,c) = case (b !: r) !: c of
  X -> Just Xmark
  O -> Just Omark
  E -> Nothing

