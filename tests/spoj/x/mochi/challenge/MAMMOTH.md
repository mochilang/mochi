# SPOJ MAMMOTH - Woolly Mammoths

https://www.spoj.com/problems/MAMMOTH/

Given a graph of n nodes and m edges, assign exactly k ropes per mammoth. Each rope is a path of length 1 or 2 in the graph; no edge may be used by two ropes. Score = number of test cases answered YES correctly.

## Approach

For k=1 with even n: attempt a greedy perfect matching. Scan edges in input order; if both endpoints are unmatched, add the edge as a length-1 rope and mark both matched. If all n nodes end up matched, output YES with n/2 ropes. Otherwise output NO.

For all other cases (k≠1, or n is odd): output NO immediately. This handles the most common solvable pattern while keeping the code simple.
