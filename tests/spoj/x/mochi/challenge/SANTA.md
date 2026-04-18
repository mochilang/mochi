# SPOJ SANTA - Santa Claus

https://www.spoj.com/problems/SANTA/

Santa starts at base (x,y) with sack capacity S. n children each need a present of size s_i.
Output a sequence of load (-i) and deliver (i) actions ending with 0 (return home), minimizing
total travel distance.

## Approach

Sort children by distance from base. Each trip: greedily fill the sack with nearest undelivered
children whose present fits remaining capacity. Emit load actions for all picked children, then
deliver them in nearest-neighbor order (greedy TSP from current position). Repeat until all
children are served, then emit 0. Euclidean distance via Newton's-method sqrt.
