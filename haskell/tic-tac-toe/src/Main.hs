module Main where

import Board
import Game (initialState)
import Display (eventHandler, displayGame)

import Graphics.Gloss

main :: IO ()
main =
  play FullScreen white 1 initialState displayGame eventHandler noAnimiate

noAnimiate :: Float -> GameState -> GameState
noAnimiate = flip const
