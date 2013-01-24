module Board where
import Data.List

emptyBoard = ["1","2","3","4","5","6","7","8","9"]

availableSpaces board = dropWhile isPiece board

placeMove :: (String, String) -> [String] -> [String]
placeMove (space, piece) board = map (swap (space,piece)) $ board

swap :: (String,String) -> String -> String
swap spaceAndPiece boardSpace = if boardSpace == fst spaceAndPiece
                                  then snd spaceAndPiece
                                  else boardSpace

full board = null (availableSpaces board)

winner board | haveWinner (rows board)      = matchingSymbol (rows board)
             | haveWinner (columns board)   = matchingSymbol (columns board)
             | haveWinner (diagonals board) = matchingSymbol (diagonals board)
             | otherwise                    = ""

haveWinner rowsOrColumns = any allEqual rowsOrColumns

matchingSymbol rowsOrColumns = head (head (filter allEqual $ rowsOrColumns))

allEqual list = length list == (length $ takeWhile (== head list) list)

isPiece a = a == "x" || a == "o"

diagonals board = [[board!!0,board!!4,board!!8],[board!!2,board!!4,board!!6]]
rows board = [(take 3 board),(take 3 (drop 3 board)),(drop 6 board)]
columns board = [[(board!!0),(board!!3),(board!!6)],
                 [(board!!1),(board!!4),(board!!7)],
                 [(board!!2),(board!!5),(board!!8)]]

toString a = [a]
