module Game where

import Data.Array

data Player = PlayerO1 | PlayerO2 deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

n :: Int
n=7

screenWidth :: Int
screenWidth = 720

screenHeight :: Int
screenHeight = 720

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

-- initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
--                    , gamePlayer = PlayerO1
--                    , gameState = Running
--                    }
-- 	where indexRange = ((0, 0), (6 , 6))