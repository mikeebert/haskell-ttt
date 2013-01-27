module ConsoleUi where

uiGreet = putStrLn "Welcome to Tic Tac Toe"

uiGetPlayer piece (computer, human) = do
  putStrLn ("Would you like player " ++ piece ++ " to be Human or Computer?")
  putStrLn ("Enter " ++ computer ++ " for Computer or " ++ human ++ " for Human:")
  input <- getLine
  if input == "1" || input == "2"
    then return input
    else uiGetPlayer piece (computer, human)

uiGetMove availableMoves = do
  putStrLn "Please select your move: "
  input <- getLine
  if any (== input) availableMoves
    then return input
    else uiGetMove availableMoves

uiDisplay string =  do
  putStrLn string


