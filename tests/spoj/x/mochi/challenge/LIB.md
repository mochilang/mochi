# SPOJ LIB - Library

https://www.spoj.com/problems/LIB/

Read t publication titles in all-lowercase. Output each in title case (first letter of every word capitalized) followed by a period. The original problem asks for a Brainfuck program, but since we implement in Mochi we perform the conversion directly.

## Approach

Walk each character; when at the start of a word (after a space or at position 0), uppercase the letter using a lookup function. Append a '.' at the end of each title.
