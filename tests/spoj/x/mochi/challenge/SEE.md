SPOJ SEE - Seedlings (Challenge)

Checkerboard baseline: BFS from (0,0) to find accessible cells, then place
1x1 shelves (type 0) on all accessible non-door free cells where (r+c)%2==1.
Even cells remain free forming a connected walking path for accessibility.
