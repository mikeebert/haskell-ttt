module Board where

import Prelude
import Data.List
import Data.Char

emptyBoard = ["1","2","3","4","5","6","7","8","9"]

availableSpaces board = filter isAvailable board

placeMove :: (String, String) -> [String] -> [String]
placeMove (space, piece) board = map (swap (space,piece)) board

swap :: (String,String) -> String -> String
swap spaceAndPiece boardSpace = if boardSpace == fst spaceAndPiece
                                  then snd spaceAndPiece
                                  else boardSpace

winner board | haveWinner (rows board)      = winningSymbol (rows board)
             | haveWinner (columns board)   = winningSymbol (columns board)
             | haveWinner (diagonals board) = winningSymbol (diagonals board)
             | otherwise                    = ""

haveWinner lines = any allEqual lines

winningSymbol :: [[String]] -> String
winningSymbol rowsOrColumns = head (head (filter allEqual rowsOrColumns))

hasWinner board = null (winner board) == False

tieGame board = null (winner board) && full board

full board = null (availableSpaces board)

allEqual list = length list == (length $ takeWhile (== head list) list)

rows board = [(take 3 board),
              (take 3 (drop 3 board)),
              (drop 6 board)]

columns board = [[(board!!0),(board!!3),(board!!6)],
                 [(board!!1),(board!!4),(board!!7)],
                 [(board!!2),(board!!5),(board!!8)]]

diagonals board = [[board!!0,board!!4,board!!8],
                   [board!!2,board!!4,board!!6]]

isAvailable a = isDigit (head a)
