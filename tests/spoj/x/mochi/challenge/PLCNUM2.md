# Place the Numbers II

**Slug:** `PLCNUM2` | **Section:** challenge | **Rank:** 4246
**URL:** https://www.spoj.com/problems/PLCNUM2/

Fill N×N board with numbers 1..N² to maximize sum of Manhattan distances
between consecutive numbers (1→2, 2→3, ..., N²→1).
Strategy: checkerboard split — odd numbers on cells where (row+col) is even,
even numbers on cells where (row+col) is odd, both filled in row-major order.
This ensures consecutive numbers always land on opposite-color checkerboard cells.
Score = 1 + SOD - minSOD.
