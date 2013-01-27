module ConsoleRunner where

import ConsoleUi
import Player
import Board
import Ai
import Data.List

computer = "computer"
computerSelection = "1"
human = "human"
humanSelection = "2"
playerOptions = (computerSelection, humanSelection)
x = "x"
o = "o"

start = do
  uiGreet
  player1type <- uiGetPlayer x playerOptions
  player2type <- uiGetPlayer o playerOptions
  let player1 = setupPlayer x player1type 
      player2 = setupPlayer o player2type
  putStrLn "got here"
  {-gameLoop player1 player2 emptyBoard-}

{-gameLoop player nextPlayer board = do-}
  {-let move         = getNextMove player nextPlayer board-}
      {-updatedBoard = placeMove (move, (piece player)) board-}
  {-uiDisplay (formatted updatedBoard)-}
  {-if gameOver updatedBoard-}
    {-then endOfGame updatedBoard-}
    {-else gameLoop nextPlayer player updatedBoard-}


{-getNextMove :: Player -> Player -> [String] -> String-}
{-getNextMove player nextPlayer board | (kind player) == computer = getAiMove board player nextPlayer-}
                                    {-| (kind player) == human    = getHumanMove board-}

{-getHumanMove board = uiGetMove (availableSpaces board)-}
{-getHumanMove board = liftM (uiGetMove (availableSpaces board))-}

gameOver board = full board || null (winner board) /= True

endOfGame board = putStrLn "Game over." 

formatted :: [String] -> String
formatted board = let rowsAsString = map concat (rows board)
                  in unlines (map (intersperse ' ') rowsAsString)

selectionFrom input | input == computerSelection = computer
                    | input == humanSelection = human
