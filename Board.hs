module Board where
import Data.Char

emptyBoard :: [Char]
emptyBoard = ['1','2','3','4','5','6','7','8','9']

availableSpaces board = filter isDigit board

placeMove :: (Char, Char) -> [Char] -> [Char]
placeMove (space, piece) board = map (swap (space,piece)) $ board

{-swap :: (Char,Char) -> Char -> Char-}
swap spaceAndPiece boardSpace = if boardSpace == fst spaceAndPiece
                                  then snd spaceAndPiece
                                  else boardSpace

full board = any isDigit board == False

winner board | all isPiece (firstRow board)  = head board
             | all isPiece (secondRow board) = head (secondRow board)
             | all isPiece (thirdRow board)  = last board
             | otherwise                     = '0'

isPiece a = a == 'x' || a == 'o'

firstRow board  = [(board!!0), (board!!1), (board!!2)]
secondRow board = [(board!!3), (board!!4), (board!!5)]
thirdRow board  = [(board!!6), (board!!7), (board!!8)]
