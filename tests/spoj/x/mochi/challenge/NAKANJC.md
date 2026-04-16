# SPOJ NAKANJC - Minimum Knight Moves Challenge

https://www.spoj.com/problems/NAKANJC/

Find the minimum number of knight moves between any two squares on an 8x8 chessboard. Input is T test cases each with start and destination squares like "a1" or "h8".

## Approach

Precompute a 64x64 distance table using BFS from each of the 64 squares. Parse square positions by looking up the column letter in "abcdefgh" and row digit in "12345678". Answer each query with a direct lookup.
