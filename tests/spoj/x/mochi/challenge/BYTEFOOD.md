# SPOJ BYTEFOOD - Byte Food

https://www.spoj.com/problems/BYTEFOOD/

Johnny starts at home (p,q) at time 0 and wants to visit shops to buy as much food as possible,
returning home by minute m. Each shop i is at (xi,yi) with Manhattan distance travel. At arrival
time t, shop i has max(0, ai - bi*t) food available; Johnny buys bi food/minute and can stay at
most ci minutes.

## Approach

Greedy simulation: at each step, pick the unvisited shop that maximizes food bought in this visit,
considering the constraint that Johnny must be able to return home by minute m. Travel uses
Manhattan distance. For each candidate shop, compute arrival time, food available, maximum stay
(min(ci, m - arrival - return_dist)), and food bought (min(stay * bi, food_available)). Pick the
best and repeat until no profitable shop can be visited.
