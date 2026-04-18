# Chess Variants Showdown

https://www.spoj.com/problems/CHE/

Place chess pieces on a board to check all kings with minimum total cost. Score is the total cost (lower is better).

## Approach

For each king on the board, find the cheapest piece that can be placed on an adjacent (or knight-distance) empty square to check it. The 8 piece costs correspond to: bishop, rook, lance, gold general, silver general, knight, pawn, king. Greedily assign one piece per king using the minimum-cost piece that covers it via an adjacent empty square.
