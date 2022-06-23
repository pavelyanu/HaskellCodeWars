# Haskell Code Wars

Project for Non-Procedural-Programming.

## Project description:

Clash of robots. Each of the players will write a program which their robots will then execute. The robots are then released into an arena and each of them begins to behave according to its program. The goal is to destroy the opponent's robots - whoever survives, wins. 

## Rules:

Robot 1 starts in the upper left corner of the arena, while robot 2 starts in the lower right corner.

The way the language interpreter and parser work is the following:
 
- It has the following types of expressions:
Constants of type Int, Bool or None
Variables
Arithmetic expression + - / * % as well as unary  minus
Comparison expressions == > >= < <=
Logical expressions ! && ||
Functions Look XPosition YPosition Direction Random (Produces random number)
 
- The language has the following statements :   
if (/expression/) {/statement/} else {/statement/}
if (/expression/) {/statement/}
while (/expression/) {/statement/}
TurnLeft
TurnRight
Move
Punch
Seq (sequence of statements)
 
- Every statement except for ifs and while should end with the semi column.
 
- A variable which was not assigned a value evaluates to None. Any operation except '==' with None will result in an error.
- If there is no opponent in the direction robot is looking in Look evaluates to -1 otherwise it evaluates to the distance to the opponent. Direction evaluates to int values with the following meanings:
- 1 — Up;
- 2 — Right;
- 3 — Down;
- 4 — Left.
 
- Each one of TurnLeft, TurnRight, Move, Punch results in overwriting the current move written in the memory. The default move is NoMove.
 
- Moving outside the board or into an opponent’s square results in not moving at all. Punches are processed before movement.
 
- Moves written in memory, as well as variables, persist between time steps.
 
## How to run:

```
./Main [player1 code] [player2 code] [time limit] [board size] [optional seed] [optional number of simulations (requires seed)]
```


If seed and number of simulations are given the game will be run multiple times with the seed incrementing between simulaitons.


