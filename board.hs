module Rendering where

import Data.Array

import Graphics.Gloss

import game

boardGridColor = makeColorI 255 255 255 255
playerO1Color = makeColorI 255 50 50 255
playerO2Color = makeColorI 50 100 255 255
tieColor = greyN 0.5

boardAsRunningPicture board =
    pictures [ color playerO1Color $ o1CellsOfBoard board
             , color playerO2Color $ o2CellsOfBoard board
             , color boardGridColor $ boardGrid
             ]

backgroundColor1 (Just PlayerO1) = playerO1Color
backgroundColor1 (Just PlayerO2) = playerO2Color
backgroundColor1 Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

o1Cell :: Picture
o1Cell = circleSolid radius1 
    where radius1 = min cellWidth cellHeight * 0.38

o2Cell :: Picture
o2Cell = circleSolid radius2
    where radius2 = min cellWidth cellHeight * 0.38

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

o1CellsOfBoard :: Board -> Picture
o1CellsOfBoard board = cellsOfBoard board (Just PlayerO1) o1Cell

o2CellsOfBoard :: Board -> Picture
o2CellsOfBoard board = cellsOfBoard board (Just PlayerO2) o2Cell

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

boardAsPicture board =
    pictures [ o1CellsOfBoard board
             , o2CellsOfBoard board
             , boardGrid
             ]

boardAsGameOverPicture winner board = color (backgroundColor1 winner) (boardAsPicture board)

-- gameAsPicture :: Game -> Picture
-- gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
--                                (fromIntegral screenHeight * (-0.5))
--                                frame
--     where frame = case gameState game of
--                     Running -> boardAsRunningPicture (gameBoard game)
--                     GameOver winner -> boardAsGameOverPicture winner (gameBoard game)