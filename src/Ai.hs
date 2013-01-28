module Ai where

import Player
import Board
import Data.Char

inProgressScore = -1
tieGameScore    = 0
depthLimit      = 5
depthLimitScore = -5
maxStrategy = "max"

firstAvailableMove list = head list

getAiMove :: [String] -> Player -> Player -> String
getAiMove board player opponent = let maxPlayer = setupMax player
                                      minPlayer = setupMin opponent
                                      scores = map (minimaxScore maxPlayer minPlayer 1) (potentialMoves maxPlayer board)
                                      moves = availableSpaces board
                                      valuesOfMoves = zip scores moves
                                  in bestMove valuesOfMoves

minimaxScore :: MinimaxPlayer -> MinimaxPlayer -> Integer -> [String] -> Integer
minimaxScore player opponent depth board = let score = value board player opponent depth
                                           in if score /= inProgressScore
                                                then score
                                                else bestScore opponent (map (minimaxScore opponent player (succ depth)) 
                                                                             (potentialMoves opponent board))

potentialMoves :: MinimaxPlayer -> [String] -> [[String]]
potentialMoves player board = map (makeMove board (symbol player)) (availableSpaces board)

bestScore player scores = if strategy player == maxStrategy
                            then maximum scores
                            else minimum scores

bestMove :: [(Integer, String)] -> String
bestMove scoresMoves = let bestScore = maximum (fst (unzip scoresMoves))
                       in moveFor bestScore scoresMoves

value :: [String] -> MinimaxPlayer -> MinimaxPlayer -> Integer -> Integer
value board player opponent depth | winner board == symbol player   = score player depth
                                  | winner board == symbol opponent = score opponent depth
                                  | tieGame board                   = tieGameScore 
                                  | depth > depthLimit              = depthLimitScore
                                  | otherwise                       = inProgressScore 

score player depth = if strategy player == maxStrategy
                       then 100 - depth
                       else -100 + depth

moveFor :: Integer -> [(Integer, String)] -> String
moveFor score (x:xs) = if score == (fst x)
                         then snd x
                         else moveFor score xs

makeMove :: [String] -> String -> String -> [String]
makeMove board symbol space = placeMove (space, symbol) board

data MinimaxPlayer = MinimaxPlayer {strategy :: String,
                                    symbol :: String
                                   } deriving (Show)

setupMax player = MinimaxPlayer {strategy = "max", 
                                 symbol = (piece player)}

setupMin player = MinimaxPlayer {strategy = "min",
                                 symbol = (piece player)}


