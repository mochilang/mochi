# SPOJ PAWNS - Pawns

https://www.spoj.com/problems/PAWNS/

Given two snapshots of an n×n chessboard pawn game, reconstruct a valid sequence of moves.
White pawns move down (row increases), black pawns move up (row decreases). Captures are
diagonal forward. Score = total moves reconstructed across all test cases.

## Approach

Output "0" for every test case, meaning no move sequence is claimed. This is always accepted
(never WA) since the problem allows admitting ignorance, and gives a baseline score of 0.
