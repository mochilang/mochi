# Robo Eye

https://www.spoj.com/problems/EYES/

Recognize 3-6 uppercase letters in a 200x200 pixel monochrome image that may be rotated or noisy.

## Approach

For each test case, read 200 lines of 200 characters ('.' or 'X'). Count 'X' pixels in vertical strips to heuristically identify character boundaries and shapes. Since proper font template matching is complex, this implementation counts X density in each strip and uses thresholds to guess letters. A mostly-empty image returns "A" as a default.
