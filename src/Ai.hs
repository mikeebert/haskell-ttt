module Ai where

import Player
import Board
import Data.Char

getDumbMove list = head list

{-minimaxScore board player opponent depth = let score = value board player opponent depth-}


value :: [String] -> Player -> Player -> Integer -> Integer
value board player opponent depth | winner board == piece player      = 100 - depth
                                  | winner board == piece opponent    = -100 + depth
                                  | draw board                        = 0
                                  | otherwise                         = -1



{-getAiMove :: [String] -> Player -> Player -> String-}
{-getAiMove board player opponent = let scores = map (minimaxScore player opponent 1) -}
                                                 {-(map (makeMove newBoard (piece player)) (availableSpaces newBoard))-}
                                      {-moves = availableSpaces board-}
                                      {-valuesOfMoves = zip scores moves-}
                                  {-in bestMove valuesOfMoves-}

{-RETURNS SCORES: -}
{-map (minimaxScore player1 player1 1) (map (makeMove newBoard (piece player1)) (availableSpaces newBoard))-}

minimaxScore :: Player -> Player -> Integer -> [String] -> Integer
minimaxScore player opponent depth board = let score = value board player opponent depth
                                           in if score /= -10
                                                then score
                                                else score

                                           {-if score /= -1 -}
                                             {-then score -}
                                             {-else minimaxScore -}


bestMove :: [(Integer, String)] -> String
bestMove scoresMoves = let bestScore = maximum (fst (unzip scoresMoves))
                       in moveFor bestScore scoresMoves

moveFor score (x:xs) = if score == (fst x)
                         then snd x
                         else moveFor score xs

makeMove :: [String] -> String -> String -> [String]
makeMove board piece space = placeMove (space, piece) board
