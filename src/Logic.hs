module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game
 
-- |isCoordinateCorrect : This checks if the integer pair returned from mousePositionAsCellCoordinate are indeed on the board.
isCoordinateCorrect = inRange ((0, 0), (n - 1, n - 1))

-- |switchPlayer : This switches the player after completion of a turn.
switchPlayer game =
    case gamePlayer game of
      PlayerO1 -> game { gamePlayer = PlayerO2 }
      PlayerO2 -> game { gamePlayer = PlayerO1 }

-- |full : This groups all the buttons of a player after every turn to check whether the player can satisfy the winning criteria.
full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

-- |Winner : This function is to check whether any player has respective buttons in such a fashion that he can win the game.
-- |This function is evoked after every single turn
winner :: Board -> Maybe Player
winner board = asum $ map full $ cols1 ++ rows ++ cols  ++ diags ++ rows1 ++ rows2 ++ rows3 ++ cols2 ++ cols3
    where rows  = [[board ! (j,i) | j <- [3..6]]| i <- [0..n-1]]    --Checking if row condition is satisfied
          rows1  = [[board ! (j,i) | j <- [2..5]]| i <- [0..n-1]]
          rows2  = [[board ! (j,i) | j <- [1..4]]| i <- [0..n-1]]
          rows3  = [[board ! (j,i) | j <- [0..3]]| i <- [0..n-1]]
          cols  = [[board ! (j,i) | i <- [0..3]] | j <- [0..n-1]]		--Checking if the column condition is satisfied 
          cols1 = [[board ! (j,i) | i <- [1..4]]| j <- [0..n-1]]
          cols2 = [[board ! (j,i) | i <- [2..5]]| j <- [0..n-1]]
          cols3 = [[board ! (j,i) | i <- [1..4]]| j <- [0..n-1]]
          cols4 = [[board ! (j,i) | i <- [3..6]]| j <- [0..n-1]]
          diags = [[board ! (i,i) | i <- [0..3]] , [board ! (i,i) | i <- [1..4]] , [board ! (i,i) | i <- [2..5]] --Checking if the diagonal condition is satisfied
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

-- |countCells : This function checks how many cells are empty in the board.
countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

-- |checkGameOver : This checks if the game is over by checking the winning criteria or if its a tie
-- |As this is invoked after every turn , if the winning criteria are not yet met then the game returns to its previous state.
checkGameOver game
    | Just p <- winner board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
        where board = gameBoard game

-- |playerTurn : Depicts whose turn it is next to play the game
-- |Also invokes checkGameOver function after every turn to see if the game has a winner
-- |Otherwise it switches the player and goes on for the next turn
playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoordinate
    | isCoordinateCorrect cellCoordinate && board ! cellCoordinate == Nothing =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoordinate, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

-- |mousePositionAsCellCoordinate : This function returns the row and column number of the cell when the player presses anywhere in the cell
mousePositionAsCellCoordinate :: (Float, Float) -> (Int, Int)
mousePositionAsCellCoordinate (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

-- |slidedown: This function is given the values of row and column number from mousePositionAsCellCoordinate
-- |It takes the column number and checks if there is an empty cell beneath the row asked by player.
-- |If there is a cell empty beneath the row specified by the player , the player's button will be allocated the first empty cell in the column starting from bottom.
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

-- |transformGame : This function is responsible to change the state of the game after every turn of player.
-- |This function also checks if the game is complete or can still run after every turn.
transformGame (EventKey (MouseButton LeftButton) Up _ mousePosition) game =
    case gameState game of
      Running -> playerTurn game $ slidedown game $ mousePositionAsCellCoordinate mousePosition 
      GameOver _ -> initialGame
transformGame _ game = game
