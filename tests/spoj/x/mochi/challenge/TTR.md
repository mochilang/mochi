# SPOJ TTR - Tetris AI

https://www.spoj.com/problems/TTR/

Greedy Tetris AI: given a sequence of N pieces (I=1, L=2, J=3, Z=4, S=5, O=6), place each piece to maximize cleared lines. Field is 10 wide, 20 tall. For each piece, output rotation (0-3) and leftmost column (1-indexed).

## Approach

For each piece, enumerate all valid (rotation, column) placements by dropping the piece straight down. Score each resulting board state with a weighted heuristic:

- Aggregate column height: weight -51
- Lines cleared: weight +760
- Holes (empty cells under filled cells): weight -360
- Bumpiness (sum of adjacent height differences): weight -180

Pick the placement with the highest heuristic score. Drop the piece, clear any full rows, and record the (rotation, x) choice.

Piece definitions use standard Tetris rotations. The I piece has 2 distinct rotations; L, J have 4; Z, S have 2; O has 1.
