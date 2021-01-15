{-# LANGUAGE StandaloneDeriving #-}
module Game (GameState(..), move, initialState, Player(..), otherPlayer) where

import Triple
import Board

newtype Player = Player Mark

instance Show Player where
  show (Player Xmark) = "X player"
  show (Player Omark) = "O player"

deriving instance Eq Player

-- The state is either the player yet-to-play with the board, or the winner
data GameState = Ongoing Player Board | Winner Player Board deriving (Eq, Show)

initialState :: GameState
initialState = Ongoing (Player Xmark) emptyBoard

move :: Player -> (Tix, Tix) -> Board -> Maybe Board
move (Player p) spot b| Nothing <- ixBoard b spot = Just $ modify b p spot
move _ _ _ = Nothing

otherPlayer :: Player -> Player
otherPlayer (Player Omark) = Player Xmark
otherPlayer (Player Xmark) = Player Omark

