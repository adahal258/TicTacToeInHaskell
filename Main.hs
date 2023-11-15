module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering

window = InWindow "TicTacToe" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = do
  let window = FullScreen
      backgroundColor = makeColor 0 0 0 255
  play window backgroundColor 30 initialGame renderTheGame reset (const id)