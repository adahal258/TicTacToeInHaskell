module Game where

import Data.Array
import Data.Foldable (asum)
import Graphics.Gloss.Interface.Pure.Game

-- Game data and types
data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

--Constant Variable
n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

isTrueCord :: (Int, Int) -> Bool
isTrueCord = inRange ((0, 0), (n - 1, n - 1))

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame :: Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))

-- Logic functions

mouseEventHandler :: (Float, Float) -> (Int, Int)
mouseEventHandler (x, y) = (convertCoord y screenHeight cellHeight, convertCoord x screenWidth cellWidth)
  where
    convertCoord pos screenSize cellSize = floor ((pos + (fromIntegral screenSize * 0.5)) / cellSize)

switchPlayer :: Game -> Game
switchPlayer game
  | gamePlayer game == PlayerX = game { gamePlayer = PlayerO }
  | gamePlayer game == PlayerO = game { gamePlayer = PlayerX }

countCells :: Maybe Player -> Board -> Int
countCells cell = foldl (\acc x -> if x == cell then acc + 1 else acc) 0 . elems

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isTrueCord cellCoord && isEmptyCell cellCoord =
        let updatedBoard = updateBoard cellCoord (gamePlayer game) (gameBoard game)
        in checkState $ switchPlayer $ game { gameBoard = updatedBoard }
    | otherwise = game
  where
    board = gameBoard game
    player = gamePlayer game
    isTrueCord (x, y) = x >= 0 && y >= 0 && x < n && y < n  -- Assuming n is defined somewhere
    isEmptyCell coord = board ! coord == Nothing
    updateBoard coord mark brd = brd // [(coord, Just mark)]

emptySpace :: [Maybe Player] -> Maybe Player
emptySpace cells = foldl checkCells Nothing cells
  where
    checkCells :: Maybe Player -> Maybe Player -> Maybe Player
    checkCells acc (Just player)
      | all (== Just player) cells = Just player
      | otherwise = acc
    checkCells acc _ = acc

winnerOfGame :: Board -> Maybe Player
winnerOfGame board = asum $ map (emptySpace . map (\(i, j) -> board ! (i, j))) combinations
  where
    combinations = rows ++ cols ++ diags
    ((x1, y1), (x2, y2)) = bounds board
    n = x2 - x1 + 1

    rows :: [[(Int, Int)]]
    rows = [[(i, j) | i <- [x1..x2]] | j <- [y1..y2]]

    cols :: [[(Int, Int)]]
    cols = [[(i, j) | j <- [y1..y2]] | i <- [x1..x2]]

    diags :: [[(Int, Int)]]
    diags = [[(i, i) | i <- [x1..x2]], [(i, y2 - (i - x1)) | i <- [x1..x2]]]

checkState :: Game -> Game
checkState game =
    case winnerOfGame (gameBoard game) of
        Just p -> game { gameState = GameOver (Just p) }
        _ | countCells Nothing (gameBoard game) == 0 ->
              game { gameState = GameOver Nothing }
        _ -> game

reset :: Event -> Game -> Game
reset (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> handleRunningGame game mousePos
        GameOver _ -> initialGame
reset _ game = game

handleRunningGame :: Game -> Point -> Game
handleRunningGame game mousePos
    | gameState game == Running =
        let cellCoord = mouseEventHandler mousePos
        in playerTurn game cellCoord
    | otherwise = game

