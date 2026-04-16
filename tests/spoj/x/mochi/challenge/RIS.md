# SPOJ RIS - Rectangle packing

https://www.spoj.com/problems/RIS/

Pack K types of rectangles (with counts) into an N×N square to maximize covered area. Score = area covered / N².

## Approach

Greedy strip (shelf) packing:
1. Expand all rectangle types into individual pieces, orienting each landscape (width ≥ height) where possible and skipping any that exceed N in both dimensions.
2. Sort by height descending so tall rectangles anchor each strip.
3. Place rectangles left-to-right; when a row is full, start a new strip at the current y offset. Skip rectangles that no longer fit vertically.
4. Output placed rectangle count and corner coordinates (x1 y1 x2 y2, 0-indexed top-left to bottom-right exclusive).
