module ConsoleUi where

uiGreet = putStrLn "Hello"

uiGetPlayer piece = do
  putStrLn ("Would you like player " ++ piece ++ " to be Human or Computer?")
  putStrLn ("Enter 1 for Human or 2 for Computer:")
  input <- getLine
  if input == "1" || input == "2"
    then return input
    else uiGetPlayer piece
