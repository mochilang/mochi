# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-27 22:18 +0700
import sys
sys.set_int_max_str_digits(0)

board = [[1, 0, 1], [0, 1, 0], [1, 1, 0]]
def flipRow(b, r):
    row = 0
    while row < len(b[r]):
        b[r][row] = 1 - b[r][row]
        row = row + 1
    return b
def flipCol(b, c):
    i = 0
    while i < len(b):
        b[i][c] = 1 - b[i][c]
        i = i + 1
    return b
board = flipRow(board, 1)
board = flipCol(board, 2)
print(str(board))
