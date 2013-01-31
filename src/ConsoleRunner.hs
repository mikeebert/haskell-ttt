module ConsoleRunner where

import ConsoleUi
import Player
import Board
import Ai
import Data.List

x = "X"
o = "O"
first = "first"
second = "second"
computerSelection = "1"
humanSelection = "2"
playerOptions = (computerSelection, humanSelection)
playAgain = "y"

start = do
  uiGreet 
  player1type <- uiGetPlayer first x playerOptions
  player2type <- uiGetPlayer second o playerOptions
  let player1 = setupPlayer x (selectionFor player1type) 
      player2 = setupPlayer o (selectionFor player2type)
  gameLoop player1 player2 emptyBoard

gameLoop player nextPlayer board 
  | isComputer player = do uiDisplayBoard (formatted board)
                           aiMoveScenario player nextPlayer board
  | isHuman player    = do uiDisplayBoard (formatted board)
                           humanMoveScenario player nextPlayer board

aiMoveScenario player nextPlayer board = do
  uiDisplayComputerMoveMessage
  let move = getAiMove board player nextPlayer
      updatedBoard = placeMove (move, (piece player)) board
  checkGameStatus player nextPlayer updatedBoard

humanMoveScenario player nextPlayer board = do
  move <- uiGetMove (availableSpaces board)
  let updatedBoard = placeMove (move, (piece player)) board
  checkGameStatus player nextPlayer updatedBoard

checkGameStatus player nextPlayer board = 
  if gameOver board
    then endOfGameScenario board
    else gameLoop nextPlayer player board

endOfGameScenario board = do
  uiDisplayBoard (formatted board)
  uiGameOverMessage (winner board) 
  askToPlayAgain

askToPlayAgain = do 
  choice <- uiAskToPlayAgain
  if choice == playAgain
    then start
    else uiDisplayGoodbye

gameOver board = full board || hasWinner board 

selectionFor input | input == computerSelection = computer
                   | input == humanSelection = human

formatted :: [String] -> String
formatted board = let rowsAsSingleString = map concat (rows board)
                  in unlines (map (intersperse ' ') rowsAsSingleString)


