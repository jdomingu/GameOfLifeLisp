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

The example screen below displays the output for the Gosper Glider Gun pattern:
```
----------------------------------------
----------------------------------------
---------------------------XX-----------
--------------------------X---X---------
----------XX-------------X-----X---XX---
----------XX-------------X---X-XX--XX---
-XX---X------XX----------X-----X--------
-X-X---X-----XXX----------X---X---------
--XXXXX------XX------------XX-----------
---XXX----XX---------X-X----------------
----------XX----------XX----------------
----------------------X-----------------
----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------
-----------------------------X----------
------------------------------XX--------
-----------------------------XX---------
----------------------------------------
```
Here are some other patterns:
```
Blinker: (game-start '((0 1) (1 1) (2 1)) 3 10)
Beacon: (game-start '((1 1) (2 1) (1 2) (4 3) (3 4) (4 4)) 6 10)
Glider: (game-start '((1 0) (2 1) (0 2) (1 2) (2 2)) 20 50)
Combination: (game-start '((0 12) (1 12) (2 12) (1 6) (2 7) (0 8) (1 8) (2 8)) 20 100)
Gosper Glider Gun: (game-start '((5 1) (5 2) (6 1) (6 2) (5 11) (6 11) (7 11) (4 12) (3 13) (3 14) (8 12) (9 13) (9 14) (6 15) (4 16) (5 17) (6 17) (7 17) (6 18) (8 16) (3 21) (4 21) (5 21) (3 22) (4 22) (5 22) (2 23) (6 23) (1 25) (2 25) (6 25) (7 25) (3 35) (4 35) (3 36) (4 36)) 40 50)
```
