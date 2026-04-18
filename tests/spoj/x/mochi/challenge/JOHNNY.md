# SPOJ JOHNNY - Johnny and His Packets

https://www.spoj.com/problems/JOHNNY/

Johnny carries n packets in two hands. Assign each packet to left or right hand to minimize
the heavier hand's total weight. Output the 1-based indices of packets in the left hand,
sorted ascending. Multiple test cases until EOF.

## Approach

Selection-sort packets by weight descending. Greedily assign each (heaviest first) to the
lighter hand — this is the classic LPT (Longest Processing Time) heuristic for makespan
minimization on 2 machines. Collect and sort the left-hand indices for output.
