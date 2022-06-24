# Battleship

## Rules

```
https://www.hasbro.com/common/instruct/battleship.pdf
```

1. 10x10 Board
2. Two players
3. Each player has 4 ships
   - Length 2
   - Length 3
   - Length 4
   - Length 5
     Each ship are to be arranged either horizontally or vertically
4. Goal is to sink the opponent's battleship
5. Each player take their turn to call the coordinates
6. After each turn, coordinates will be printed out and announce if opponent ship is occupied by that coordinate.
7. When a ship is sunk, "Player X ship is sunk" will be printed out.
8. Game ends when all battleship are sunk.
9. Exception handling

## Example Input

Elton - Player 1
John - Player 2
(0,0);(0,1) - Position of Player 1 ship of length 2
(6,7);(7,7);(8,7) - Position of Player 1 ship of length 3
(3,3);(3,4);(3,5);(3,6) - Position of Player 1 ship of length 4  
(5,9);(6,9);(7,9);(8,9);(9,9) - Position of Player 1 ship of length 5
(0,0);(0,1) - Position of Player 2 ship of length 2
(1,1);(1,2);(1,3) - Position of Player 2 ship of length 3
(2,1);(3,1);(4,1);(5,1) - Position of Player 2 ship of length 4  
(3,2);(4,2);(5,2);(6,2);(7,2) - Position of Player 2 ship of length 5
(6,6)
(6,7)
(3,3)
(3,2)
(6,9)
(8,9)
(7,9)
(3,4)
(9,6)
(9,7)
(9,8)
(5,9)
(9,9)
(9,9)
(8,9)
(7,9)
(0,0)
(4,9)
(5,9)
(6,9)

# Run

'''
cabal build
cabal run
'''
