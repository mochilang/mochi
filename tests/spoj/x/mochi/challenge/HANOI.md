# SPOJ HANOI - Towers of Hanoi

https://www.spoj.com/problems/HANOI/

n towers (1..n), k target tower, m disks (sizes 1..m), max move distance d. Move all disks to tower k with minimum moves. Score = T/(T+A) where T≤20000 and A is move count. Output moves as "a b" pairs then "0 0".

## Approach

Standard iterative 3-peg Towers of Hanoi when we can find a valid auxiliary tower reachable from both source and target within distance d. The classic iterative algorithm uses the parity of move number and disk positions to generate all 2^m - 1 moves without recursion.

If d ≥ n-1 all towers are reachable from any tower. Find source (tower holding all disks initially), pick an auxiliary tower, and apply the iterative binary-encoding algorithm. For restricted d, use a nearby intermediate tower.
