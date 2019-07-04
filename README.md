# Haskell Maze Game
This game was made with the gloss library using the function play. The original Maze Generator was made by Joe Wingbermuehle, I'm using his code to generate a random Maze and then be able to play the game on an GUI. I've added and removed a lot of functions from the original file from Joe, anyway you can get the original one in the link bellow
https://github.com/joewing/maze/blob/master/maze.hs

## How to Run
```
$ cabal update
$ cabal install random
$ cabal install gloss
$ ghc lab.hs -o maze 
$ ./maze
```

![Game](https://i.imgur.com/Ax6Or6n.png)

(in windows you'll need to write maze.exe to run it)
