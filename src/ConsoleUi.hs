module ConsoleUi where

uiGreet = do
  putStrLn ("+++++++++++++++++++++++++" ++
            "\nWelcome to Tic Tac Toe!")

uiDisplayOptionMessage = do
  putStrLn ("In this game you have the option of playing Human vs. Human,\n" ++
            "Human vs. Computer, or watching two Computer players duke it out.\n")

uiGetPlayer number piece (computerChoice, humanChoice) = do
  putStrLn ("Would you like player " ++ number ++ " (" ++ piece ++ ") to be a Computer or a Human?")
  putStrLn ("Enter " ++ computerChoice ++ " for Computer or " ++ humanChoice ++ " for Human:")
  input <- getLine
  if input == "1" || input == "2"
    then return input
    else uiGetPlayer number piece (computerChoice, humanChoice)

uiGetMove availableMoves = do
  putStrLn "Please select your move: "
  input <- getLine
  if any (== input) availableMoves
    then return input
    else uiGetMove availableMoves

uiDisplayBoard board =  do
  putStrLn (board ++ "=======")

uiDisplayComputerMoveMessage = do
  putStrLn "\nComputer move:"

uiGameOverMessage winner = do
  if null winner
    then putStrLn "Tie Game. Game Over"
    else putStrLn ("Game Over: " ++ winner ++ " wins!")

uiAskToPlayAgain = do
  putStrLn ("Would you like to play again?\n" ++
            "Please answer (y) or (n):")
  input <- getLine
  if input == "y" || input == "n"
    then return input
    else uiAskToPlayAgain

uiDisplayGoodbye = putStrLn "Thanks for playing."
