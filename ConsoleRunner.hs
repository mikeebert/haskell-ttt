module ConsoleRunner where

import Board
import ConsoleUi

start = do
  uiGreet
  choice <- (uiGetPlayer "x")
  {-checkPlayerSelection "x" choice-}
  putStrLn ("Choice was " ++ choice)

{-checkPlayerSelection piece reply = if reply == "1" || reply == "2"-}
                                     {-then reply-}
                                     {-else uiGetPlayer piece-}
