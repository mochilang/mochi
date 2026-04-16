# SPOJ MATE - Materializer

https://www.spoj.com/problems/MATE/

Fill all required '#' cubes in a c×b×a grid using the materializer commands.

## Approach

Fill the entire cuboid using a serpentine traversal:
1. For each layer (bottom to top): emit N to go up (except first layer).
2. Within each layer: traverse rows in order, alternating left-to-right and right-to-left.
3. Between rows: emit M 0 1 to move down one row.
4. Within each row: emit E to fill, then M ±1 0 to move to next column.
5. End with C.

Stability is guaranteed: layer 1 cubes are always stable (w=1 rule). For higher layers, the cube directly below was already filled in the previous layer, satisfying the w-1 support condition.

Extra non-required cubes are filled as scaffolding (allowed by problem statement).

## Notes

Total commands per test: approximately 2*a*b*c + a, well within the 4*a*b*c budget.
Score = (r + 10*n_extra) / (a*b*c). With r ≈ 2abc and n_extra ≤ abc, score ≤ 12.
