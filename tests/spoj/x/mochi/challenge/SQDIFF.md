# SPOJ SQDIFF - Area Difference

https://www.spoj.com/problems/SQDIFF/

Given two sprinklers at positions (x1,y1) radius r1 and (x2,y2) radius r2, output the absolute
difference between the area watered only in the morning and area watered only in the evening,
rounded to 2 decimal places. Score is source length.

## Approach

The areas differ only in the non-overlapping parts: |area1_only - area2_only| = |π·r1² - π·r2²| = π·|r1²-r2²|.
The intersection cancels out entirely. Coordinates are irrelevant to the result.
