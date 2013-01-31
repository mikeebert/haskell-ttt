module Ai where

import Player
import Board
import MinimaxPlayer
import Data.Char

inProgressScore = -1
tieGameScore    = 0
depthLimit      = 5
depthLimitScore = -5
firstCorner     = "1"
middlePosition  = "5"

firstAvailableMove list = head list

{-Uses hard logic for first moves to speed game play. However, Minimax does work for these moves. -}
getAiMove :: [String] -> Player -> Player -> String
getAiMove board player opponent  
  | board == emptyBoard      = firstCorner
  | followsOpeningMove board = bestSecondMove board
  | otherwise = bestMoveFrom valuesAndMoves
                  where maxPlayer = setupMax (piece player)
                        minPlayer = setupMin (piece opponent)
                        scores = map (minimaxScore maxPlayer minPlayer 1) (potentialMoves maxPlayer board)
                        moves = availableSpaces board
                        valuesAndMoves = zip scores moves

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

bestMoveFrom :: [(Integer, String)] -> String
bestMoveFrom scoresMoves = moveFor bestScore scoresMoves
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

followsOpeningMove board = length (availableSpaces board) == (length emptyBoard) - 1 

bestSecondMove board = if middlePositionIsOpen board
                         then middlePosition
                         else firstCorner

middlePositionIsOpen board = any (== middlePosition) board
