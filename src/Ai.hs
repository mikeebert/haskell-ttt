module Ai where

import Player
import Board
import Data.Char

firstAvailableMove list = head list

{-getAiMove :: [String] -> Player -> Player -> String-}
getAiMove board player opponent = let scores = map (minimaxScore player opponent 1) (map (makeMove board (piece player)) (availableSpaces board))
                                      moves = availableSpaces board
                                      valuesOfMoves = zip scores moves
                                  in bestMove valuesOfMoves

minimaxScore :: Player -> Player -> Integer -> [String] -> Integer
minimaxScore player opponent depth board = let score = value board player opponent depth
                                           in if score /= -1
                                                then score
                                                else bestScore opponent (map (minimaxScore opponent player (succ depth)) 
                                                       (map (makeMove board (piece player)) (availableSpaces board)))

bestScore player scores = if strategy player == "max"
                            then maximum scores
                            else minimum scores

bestMove :: [(Integer, String)] -> String
bestMove scoresMoves = let bestScore = maximum (fst (unzip scoresMoves))
                       in moveFor bestScore scoresMoves

value :: [String] -> Player -> Player -> Integer -> Integer
value board player opponent depth | winner board == piece player   = 100 - depth
                                  | winner board == piece opponent = -100 + depth
                                  | tieGame board                  = 0
                                  | otherwise                      = -1

moveFor :: Integer -> [(Integer, String)] -> String
moveFor score (x:xs) = if score == (fst x)
                         then snd x
                         else moveFor score xs

makeMove :: [String] -> String -> String -> [String]
makeMove board piece space = placeMove (space, piece) board
