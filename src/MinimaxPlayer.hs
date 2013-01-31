module MinimaxPlayer where

maxStrategy = "max"
minStrategy = "min"
maxScore = 100
minScore = -100

data MinimaxPlayer = MinimaxPlayer {strategy :: String,
                                    symbol :: String
                                   } deriving (Show)

setupMax piece = MinimaxPlayer {strategy = maxStrategy, 
                                 symbol = piece}

setupMin piece = MinimaxPlayer {strategy = minStrategy,
                                 symbol = piece}


