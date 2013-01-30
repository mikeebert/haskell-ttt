module Player where

computer = "computer"
human = "human"

data Player = Player {
  piece         :: String,
  kind          :: String
  } deriving (Show)

setupPlayer symbol playerType = Player {piece = symbol,
                                        kind = playerType}

isComputer player = (kind player) == computer
isHuman player    = (kind player) == human
