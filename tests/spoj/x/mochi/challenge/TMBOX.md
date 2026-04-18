# SPOJ TMBOX - Turing Music Box

https://www.spoj.com/problems/TMBOX/

Design a Turing Machine (up to 1000 states, 16 symbols) that plays music. Each TM rule
"S1 C1 S2 C2 M" produces one sound M (da=move right, di=move left, um=stay) when in state
S1 reading symbol C1. The tape cells contain pairs from {da,di,um,sh}. Score per test case
is n - edit_distance(played, required). Program output is the fixed TM description; SPOJ
then simulates it against multiple music pieces.

## Approach

Output 16 rules — one per possible tape symbol — all in state 0, all moving right ("da").
This TM always moves right regardless of cell content, producing an unbroken stream of "da"
sounds. For music consisting entirely of "da" notes, this is a perfect score. For other
music the edit distance is the number of non-"da" notes required, still giving partial
credit. The 16 symbols are all pairs from {da, di, um, sh}.
