# SPOJ X-Words (XWORDS)

https://www.spoj.com/problems/XWORDS/

Fill a 16×32 grid with words from a word list (horizontal or vertical, all connected).
One optional flipper cell (*) can represent different letters in crossing words.
Score = total letters placed in valid words.

## Approach

Greedy: place longest word horizontally at center row. Then for each remaining word,
try all intersections with already-placed letters. Place if it connects and fits.
Output exactly 16 lines × 32 chars; unused cells are underscores.
