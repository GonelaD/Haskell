module Board where
import Data.Array
import Graphics.Gloss
import Game
-- |boardGridColor sets the colour of Grid.
boardGridColor = makeColorI 255 255 255 255
-- |player01Color sets the colour of dots filled by Player01.
playerO1Color = makeColorI 255 50 50 255
-- |player02Color sets the colour of dots filled by Player02. 
playerO2Color = makeColorI 50 100 255 255
-- |tieColor sets the colour displayed when the game is tied.
tieColor = greyN 0.5
-- |boardAsRunningPicture applies all the colours to the board appropriately.
boardAsRunningPicture board =
    pictures [ color playerO1Color $ o1CellsOfBoard board
             , color playerO2Color $ o2CellsOfBoard board
             , color boardGridColor $ boardGrid
             ]
-- |backgroundColor1 sets the colour of cells in GameOver state depending on the winner.
backgroundColor1 (Just PlayerO1) = playerO1Color
backgroundColor1 (Just PlayerO2) = playerO2Color
backgroundColor1 Nothing = tieColor
-- |Sets appropriate row and coloumn numbers in the picture.
snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5
-- |o1Cell function returns the Picture type.It uses inbuilt function of circleSolid which returns Picture(Fully Filled Circle) taking Float(radius) as input
o1Cell :: Picture
o1Cell = circleSolid radius1 
    where radius1 = min cellWidth cellHeight * 0.38
-- |o2Cell function returns the Picture type.It uses inbuilt function of circleSolid which returns Picture(Fully Filled Circle) taking Float(radius) as input
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
-- |boardGrid gives the grid on Background appropriately using screenwidth and screenheight defined in Main.
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
-- |boardAsGameOverPicture actually changes the color of Cells in GameOver state as set by backgroundColor1
boardAsGameOverPicture winner board = color (backgroundColor1 winner) (boardAsPicture board)
-- |gameAsPicture takes in Game and returns picture corresponding to the state of game. 
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)
