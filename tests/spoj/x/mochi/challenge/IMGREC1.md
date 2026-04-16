# SPOJ IMGREC1 - Image Recognition 1

https://www.spoj.com/problems/IMGREC1/

Distinguish "dagger" from "zero" in bicoloured images made of 'x' and '.'.

## Approach

BFS flood fill from all '.' cells on the image border. Any '.' cell unreachable from the border is an interior hole, indicating a zero ('0'). If all '.' cells are reachable from the border, the shape is solid (a dagger, 'x').

The grid is stored as a flat list with `grid[row * cols + col]` indexing.
