module Main where
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game
import Board
import Logic
-- |window sets the screenWidth and screenheight and also the background colour 
window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255
main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
