# Sudoku_Solver
Simple Sudoku Solver in Haskell (Pure Functional). Haskell数独求解器

## Algorithm
**Backtracking/Brute Force:**

The solver searches the puzzle from left to right and from top to down, which try to find a digit from 1 to 9 in each blank cell to satisfy the constraints of the game rules. If not satisfied, the solver will backtrack to the previous blank position and replace the filled digit to another new one. In this way, the solver performs the search until all potential solutions are reached and recorded. 

For more details, please read [Report.pdf](https://github.com/Chan-Xu/Sudoku_Solver/blob/master/Report.pdf)

## Testing Report
**Unsolved Easy Sudoku**

--|--|Time
--|--|--
fromString|True|0.002 sec
toString|True|0.000 sec
okSudoku|True|0.001 sec
isSudoku|True|0.000 sec
update|True|0.000 sec

**Solved Easy Sudoku**

--|--|Time
--|--|--
fromString|True|0.003 sec
toString|True|0.000 sec
okSudoku|True|0.002 sec
noBlanks|True|0.000 sec
solve|True|0.002 sec

**Unsolved Hard Sudoku**

--|--|Time
--|--|--
fromString|True|0.005 sec
toString|True|0.001 sec
okSudoku|True|0.002 sec
isSudoku|True|0.000 sec
update|True|0.001 sec

**Solved Hard Sudoku**

--|--|Time
--|--|--
fromString|True|0.005 sec
toString|True|0.001 sec
okSudoku|True|0.003 sec
noBlanks|True|0.001 sec
solve|True|0.004 sec



