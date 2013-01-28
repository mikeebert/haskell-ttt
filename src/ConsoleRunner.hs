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

gameLoop player nextPlayer board 
  | isComputer player = do uiDisplay (formatted board)
                           aiMoveScenario player nextPlayer board
  | isHuman player    = do uiDisplay (formatted board)
                           humanMoveScenario player nextPlayer board

aiMoveScenario player nextPlayer board = do
  let move = getAiMove board player nextPlayer
      updatedBoard = placeMove (move, (piece player)) board
  checkGameStatus player nextPlayer updatedBoard

humanMoveScenario player nextPlayer board = do
  move <- uiGetMove (availableSpaces board)
  let updatedBoard = placeMove (move, (piece player)) board
  checkGameStatus player nextPlayer updatedBoard

checkGameStatus player nextPlayer board = if gameOver board
                                            then endOfGame board
                                            else gameLoop nextPlayer player board

endOfGame board = do
  uiDisplay (formatted board)
  uiGameOverMessage (winner board) 

isComputer player = (kind player) == computer
isHuman player    = (kind player) == human

gameOver board = hasWinner board || full board 

formatted :: [String] -> String
formatted board = let rowsAsString = map concat (rows board)
                  in unlines (map (intersperse ' ') rowsAsString)

selectionFor input | input == computerSelection = computer
                   | input == humanSelection = human
