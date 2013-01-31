##Tic Tac Toe in Haskell

1. Download and install [the Haskell
   Platform](http://www.haskell.org/platform/) if you don't have it yet.
2. Download, clone or unzip the tic-tac-toe source code. 

###Play Game
- `cd` into the project directory and type the following command in your terminal: 

```
runhaskell -isrc TicTacToe.hs
```

###Run Specs
- Install Hspec from the terminal:  
```
cabal update && cabal install hspec
```

- Type the following command in your terminal (from the project directory)
```
runhaskell -itest -isrc test/TestRunner.hs
```

