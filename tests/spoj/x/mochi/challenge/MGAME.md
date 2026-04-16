# SPOJ MGAME - Multiplayer Game

https://www.spoj.com/problems/MGAME/

n players each with a shoot-list of enemies. Each second every living player shoots one enemy from their list. Shot players die instantly. Maximize tmax/tmin - 1 where tmax = longest possible game and tmin = shortest.

## Approach

**Short game:** each round, every alive player shoots the first alive enemy on their list. All targeted players die simultaneously. Repeat until ≤1 player remains.

**Long game:** each round, count how many alive players can shoot each alive target. Pick the target hit by the most players — everyone who can shoot that target does so, causing exactly 1 death while wasting the rest of the shots. Other players shoot their first alive target (these deaths also occur). This greedy approach maximizes "wasted" shots to minimize deaths per round.

Output: "case C Y", then tmax, then tmax lines of shots for each long-game round, then tmin, then tmin lines of shots for each short-game round. Each line lists the target (1-indexed) each surviving player shoots, in player order.
