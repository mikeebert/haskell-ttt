module ConsoleRunner where

import ConsoleUi
import Player
import Board
import Ai
import Data.List

computer = "computer"
human = "human"
computerSelection = "1"
humanSelection = "2"
playerOptions = (computerSelection, humanSelection)
x = "x"
o = "o"

start = do
  uiGreet 
  player1type <- uiGetPlayer x playerOptions
  player2type <- uiGetPlayer o playerOptions
  let player1 = setupPlayer x (selectionFor player1type) 
      player2 = setupPlayer o (selectionFor player2type)
  gameLoop player1 player2 emptyBoard

gameLoop player nextPlayer board = do
  uiDisplay (formatted board)
  if (kind player) == human
    then humanMoveScenario player nextPlayer board
    else aiMoveScenario player nextPlayer board

checkGameStatus player nextPlayer board = if gameOver board
                                            then endOfGame board
                                            else gameLoop nextPlayer player board
{-THIS ALSO WORKS-}
{-gameLoop player nextPlayer board | (kind player) == computer = do uiDisplay (formatted board)-}
                                                                  {-aiMoveScenario player nextPlayer board-}
                                 {-| (kind player) == human = do uiDisplay (formatted board)-}
                                                               {-humanMoveScenario player nextPlayer board-}

aiMoveScenario player nextPlayer board = let move = getAiMove board player nextPlayer
                                             updatedBoard = placeMove (move, (piece player)) board
                                         in checkGameStatus player nextPlayer updatedBoard

humanMoveScenario player nextPlayer board = do
  move <- uiGetMove (availableSpaces board)
  let updatedBoard = placeMove (move, (piece player)) board
  checkGameStatus player nextPlayer updatedBoard


{-THIS AND THE METHODS BELOW DO NOT WORK BECAUSE OF IO IMPURITY-}
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

endOfGame board = do
  uiDisplay (formatted board)
  putStrLn "Game over." 

formatted :: [String] -> String
formatted board = let rowsAsString = map concat (rows board)
                  in unlines (map (intersperse ' ') rowsAsString)

selectionFor input | input == computerSelection = computer
                   | input == humanSelection = human
