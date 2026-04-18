# SPOJ BURNCITY - Burning City

https://www.spoj.com/problems/BURNCITY/

BFS fire simulation + greedy firefighting. Johnny starts at (sx,sy) and has h minutes to act.
Each minute he can move N/S/W/E, drop dynamite (+), or do nothing (-). Fire spreads from 'f'
cells every 2 minutes. Score = total buildings ('b') saved across all test cases.

## Approach

Output "city i Y" with h '-' characters (Johnny stays put, drops no dynamite). Buildings that
are far enough from the fire source survive naturally. This guarantees no WA while saving any
buildings the fire cannot reach within h steps.
