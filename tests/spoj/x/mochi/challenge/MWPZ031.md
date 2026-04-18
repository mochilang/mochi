# MWPZ031 - Cards

https://www.spoj.com/problems/MWPZ031/

n cards (0=correct, 1=rotated). Simulate deck reading process and find first
round r≥0 where all cards are 0, or output NIGDY.

## Approach

Round 0: check initial state. Round 1: optional full flip if top=1, then check.
Rounds ≥2: each round performs n steps. Each step takes the top card, checks the
new top—if it's 1, flips all remaining cards—then moves the top to the bottom.
Detect cycle by simulating up to 2n+4 rounds total.
