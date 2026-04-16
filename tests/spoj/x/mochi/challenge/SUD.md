SPOJ SUD - Sudoku (rank 1414)

Iterative backtracking solver for 9x9 Sudoku. Reads 81-character strings ('1'-'9' or '.') and outputs the solved grid. Uses a trail of (position, next-candidate) pairs to simulate recursion without actual recursion. At each empty cell, tries digits 1-9 in order; on failure backtracks to the previous decision point and tries the next candidate.
