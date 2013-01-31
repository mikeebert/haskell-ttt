module Ai where

import Player
import Board
import MinimaxPlayer
import Data.Char

inProgressScore = -1
tieGameScore    = 0
depthLimit      = 5
depthLimitScore = -5

firstAvailableMove list = head list

getAiMove :: [String] -> Player -> Player -> String
getAiMove board player opponent = 
  bestMove valuesOfMoves
  where maxPlayer = setupMax (piece player)
        minPlayer = setupMin (piece opponent)
        scores = map (minimaxScore maxPlayer minPlayer 1) (potentialMoves maxPlayer board)
        moves = availableSpaces board
        valuesOfMoves = zip scores moves

minimaxScore :: MinimaxPlayer -> MinimaxPlayer -> Integer -> [String] -> Integer
minimaxScore player opponent depth board = 
  let score = value board player opponent depth
  in if score /= inProgressScore
       then score
       else bestScore opponent (map (minimaxScore opponent player (succ depth)) 
                                      (potentialMoves opponent board))

potentialMoves :: MinimaxPlayer -> [String] -> [[String]]
potentialMoves player board = map (makeMove board (symbol player)) (availableSpaces board)

bestScore player scores 
  | strategy player == maxStrategy = maximum scores
  | strategy player == minStrategy = minimum scores

bestMove :: [(Integer, String)] -> String
bestMove scoresMoves = moveFor bestScore scoresMoves
                       where bestScore = maximum (fst (unzip scoresMoves))

moveFor :: Integer -> [(Integer, String)] -> String
moveFor score (x:xs) = if score == (fst x)
                         then snd x
                         else moveFor score xs

value :: [String] -> MinimaxPlayer -> MinimaxPlayer -> Integer -> Integer
value board player opponent depth | winner board == symbol player   = score player depth
                                  | winner board == symbol opponent = score opponent depth
                                  | tieGame board                   = tieGameScore 
                                  | depth > depthLimit              = depthLimitScore
                                  | otherwise                       = inProgressScore 

score player depth
  | strategy player == maxStrategy = maxScore - depth
  | strategy player == minStrategy = minScore + depth

makeMove :: [String] -> String -> String -> [String]
makeMove board symbol space = placeMove (space, symbol) board

