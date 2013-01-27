module Player where

data Player = Player {
  piece         :: String,
  kind          :: String
  } deriving (Show)

setupPlayer symbol playerType = Player {piece = symbol,
                                        kind = playerType}

