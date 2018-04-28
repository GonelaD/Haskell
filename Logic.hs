module Logic1 where

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
winner board = asum $ map full $ cols1 ++ rows ++ cols  ++ diags ++ rows1++rows2++rows3 ++ cols2 ++ cols3
    where rows  = [[board ! (j,i) | j <- [3..6]]| i <- [0..n-1]]
          rows1  = [[board ! (j,i) | j <- [2..5]]| i <- [0..n-1]]
          rows2  = [[board ! (j,i) | j <- [1..4]]| i <- [0..n-1]]
          rows3  = [[board ! (j,i) | j <- [0..3]]| i <- [0..n-1]]
          cols  = [[board ! (j,i) | i <- [0..3]] | j <- [0..n-1]]
          cols1 = [[board ! (j,i) | i <- [1..4]]| j <- [0..n-1]]
          cols2 = [[board ! (j,i) | i <- [2..5]]| j <- [0..n-1]]
          cols3 = [[board ! (j,i) | i <- [1..4]]| j <- [0..n-1]]
          cols4 = [[board ! (j,i) | i <- [3..6]]| j <- [0..n-1]]
          diags = [[board ! (i,i) | i <- [0..3]] , [board ! (i,i) | i <- [1..4]] , [board ! (i,i) | i <- [2..5]] 
                  ,[board ! (i,i) | i <- [3..6]] , [board ! (i,j) | i <- [0..3] , let j=n-2-i] 
                  ,[board ! (i,j) | i <- [1..4] , let j=n-2-i] ,[board ! (i,j) | i <- [0..3] , let j=n-4-i]
                  ,[board ! (i,j) | i <- [2..5] , let j=n-2-i] , [board ! (i,j) | i <- [0..3] , let j = n-3-i]
                  ,[board ! (i,j) | i <- [1..4] , let j=n-3-i] , [board ! (i,j) | i <- [2..5] , let j = n-i]
                  ,[board ! (i,j) | i <- [0..3] , let j=n-3-i] , [board ! (i,j) | i <- [1..4] , let j = n-i] 
                  ,[board ! (i,j) | i <- [3..6] , let j=n-i] , [board ! (i,j) | i <- [2..5] , let j = n+1-i]
                  ,[board ! (i,j) | i <- [3..6] , let j=n+1-i] , [board ! (i,j) | i <- [3..6] , let j=n+2-i]
                  ,[board ! (i,j) | i <- [3..6] , let j=i-3] , [board ! (i,j) | i <- [3..6] , let j=i-2] 
                  ,[board ! (i,j) | i <- [2..5] , let j=i-2]  , [board ! (i,j) | i <- [3..6] , let j=i-1]
                  ,[board ! (i,j) | i <- [2..5] , let j=i-1] , [board ! (i,j) | i <- [1..4] , let j=i-1]
                  ,[board ! (i,j) | i <- [0..3] , let j=i+1] , [board ! (i,j) | i <- [1..4] , let j=i+1]
                  ,[board ! (i,j) | i <- [2..5] , let j=i+1] , [board ! (i,j) | i <- [0..3] , let j=i+2]
                  ,[board ! (i,j) | i <- [1..4] , let j=i+2] , [board ! (i,j) | i <- [0..3] , let j=i+3]
                  ,[board ! (i,j) | i <- [0..3] , let j = n-1-i ],[board ! (i,j) | i <- [1..4] , let j= n-1-i] 
                  ,[board ! (i,j) | i <- [2..5] , let j = n-1-i ],[board ! (i,j) | i <- [3..6] , let j= n-1-i]]

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
slidedown :: Game -> (Int, Int) -> (Int, Int)
slidedown game (x,y)
    | board ! (0, y) == Nothing =
        (0,y)
    | board ! (1, y) == Nothing =
        (1,y)
    | board ! (2, y) == Nothing =
        (2,y)
    | board ! (3, y) == Nothing =
        (3,y)
    | board ! (4, y) == Nothing =
        (4,y)
    | board ! (5, y) == Nothing =
        (5,y)
    | otherwise=
        (6,y)
    where board = gameBoard game


transformGame (EventKey (MouseButton LeftButton) Up _ mousePosition) game =
    case gameState game of
      Running -> playerTurn game $ slidedown game $ mousePositionAsCellCoordinate mousePosition 
      GameOver _ -> initialGame
transformGame _ game = game