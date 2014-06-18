Game-of-Life-Lisp
=================

A very simple command line implementation of Conway's Game of Life for Common Lisp.

To run the program, complete the following steps:

1. Open the Common Lisp interpreter. (This has only been tested on Gnu Clisp.)
2. Run the following command to load the program: `(load "game-of-life.lsp")`
3. Run the `game-start` function. For example, you might enter the following command:
```      
(game-start '((1 0) (2 1) (0 2) (1 2) (2 2)) 20 50)
```
   
The command displays a screen of ASCII art that refreshes every 0.15 seconds.

The game-start function takes the following three paramters:
- live-list. A list of coordinates for live cells. For example: '((0 1) (1 1) (2 1))
- size-sq. The size of the square grid. For example, enter 20 to draw a 20x20 grid.
- num-gen. The number of generations to simulate.

The example game-start command above shows a glider making its way across the screen:
```
--------------------
--------------------
--X-----------------
---X----------------
-XXX----------------
--------------------
--------------------
--------------------
--------------------
```
Here are some other patterns:
```
Blinker: (game-start '((0 1) (1 1) (2 1)) 3 10)
Beacon: (game-start '((1 1) (2 1) (1 2) (4 3) (3 4) (4 4)) 6 10)
Glider: (game-start '((1 0) (2 1) (0 2) (1 2) (2 2)) 20 50)
Combination: (game-start '((0 12) (1 12) (2 12) (1 6) (2 7) (0 8) (1 8) (2 8)) 20 100)
```
