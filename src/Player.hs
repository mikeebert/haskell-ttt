module Player where

data Player = Player {
  strategy      :: String,
  piece         :: String,
  startingScore :: Integer
  } deriving (Show)


