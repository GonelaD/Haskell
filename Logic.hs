module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

isCoordinateCorrect = inRange ((0, 0), (n - 1, n - 1))

switchPlayer game =
    case gamePlayer game of
      PlayerO1 -> game { gamePlayer = PlayerO2 }
      PlayerO2 -> game { gamePlayer = PlayerO1 }

full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

winner :: Board -> Maybe Player
winner board = asum $ map full $ cols1 ++ rows ++ cols ++ rows1 ++ rows2 ++ rows3 ++ cols2 ++ cols3
    where rows  = [[board ! (j,i) | j <- [3..6]]| i <- [0..n-1]]
          rows1  = [[board ! (j,i) | j <- [2..5]]| i <- [0..n-1]]
          rows2  = [[board ! (j,i) | j <- [1..4]]| i <- [0..n-1]]
          rows3  = [[board ! (j,i) | j <- [0..3]]| i <- [0..n-1]]
          cols  = [[board ! (j,i) | i <- [0..3]] | j <- [0..n-1]]
          cols1 = [[board ! (j,i) | i <- [1..4]]| j <- [0..n-1]]
          cols2 = [[board ! (j,i) | i <- [2..5]]| j <- [0..n-1]]
          cols3 = [[board ! (j,i) | i <- [3..6]]| j <- [0..n-1]]
          

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

checkGameOver game
    | Just p <- winner board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
        where board = gameBoard game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoordinate
    | isCoordinateCorrect cellCoordinate && board ! cellCoordinate == Nothing =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoordinate, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

mousePositionAsCellCoordinate :: (Float, Float) -> (Int, Int)
mousePositionAsCellCoordinate (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )


transformGame (EventKey (MouseButton LeftButton) Up _ mousePosition) game =
    case gameState game of
      Running -> playerTurn game  $ mousePositionAsCellCoordinate mousePosition 
      GameOver _ -> initialGame
transformGame _ game = game