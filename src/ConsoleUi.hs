module ConsoleUi where

uiGreet = do
  putStrLn ("+++++++++++++++++++++++++" ++
            "\nWelcome to Tic Tac Toe!\n\n" ++ 
            "In this game you have the option of playing Human vs. Human,\n" ++
            "Human vs. Computer, or watching two Computer players duke it out.\n")

uiGetPlayer number piece (computer, human) = do
  putStrLn ("Would you like the " ++ number ++ " player (" ++ piece ++ ") to be Human or Computer?")
  putStrLn ("Enter " ++ computer ++ " for Computer or " ++ human ++ " for Human:")
  input <- getLine
  if input == "1" || input == "2"
    then return input
    else uiGetPlayer number piece (computer, human)

uiGetMove availableMoves = do
  putStrLn "Please select your move: "
  input <- getLine
  if any (== input) availableMoves
    then return input
    else uiGetMove availableMoves

uiDisplayBoard board =  do
  putStrLn ("=======\n" ++ board ++ "=======\n")

uiDisplayComputerMoveMessage = do
  putStrLn "Computer move..."

uiGameOverMessage winner = do
  if null winner
    then putStrLn "Tie Game. Game Over"
    else putStrLn ("Game Over: " ++ winner ++ " wins!")

uiAskToPlayAgain = do
  putStrLn ("Would you like to play again?\n" ++
            "Please answer (Y) or (N):")
  input <- getLine
  if input == "Y" || input == "N"
    then return "yes"
    else uiAskToPlayAgain

uiDisplayGoodbye = putStrLn "Thanks for playing."
