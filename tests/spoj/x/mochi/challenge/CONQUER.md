# CONQUER - Conquest of the Grid

https://www.spoj.com/problems/CONQUER/

Two kings on a 3D grid (X and O). Each attack captures one entire 26-connected
component of enemy territory. King1 (X) goes first. Determine winner with
optimal play.

## Approach

Count connected components: nX for X, nO for O (26-connectivity = sharing edge
or corner in 3D). King1 wins iff nX ≥ nO: King1 eliminates one O-component per
turn, King2 eliminates one X-component per turn, King1 finishes first when
nO ≤ nX. BFS over 3D grid with 26 neighbors.
