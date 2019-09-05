# Sudoku_Solver
Simple Sudoku Solver in Haskell (Pure Functional). Haskell数独求解器

## Algorithm
**Backtracking/Brute Force:**

The solver searches the puzzle from left to right and from top to down, which try to find a digit from 1 to 9 in each blank cell to satisfy the constraints of the game rules. If not satisfied, the solver will backtrack to the previous blank position and replace the filled digit to another new one. In this way, the solver performs the search until all potential solutions are reached and recorded. 

For more details, please read [Report.pdf](https://github.com/Chan-Xu/Sudoku_Solver/blob/master/Report.pdf)
