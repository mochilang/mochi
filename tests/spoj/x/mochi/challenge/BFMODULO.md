# SPOJ BFMODULO - a mod b in Mochi

https://www.spoj.com/problems/BFMODULO/

Read lines of "aaa b" until EOF, where aaa is a 3-digit integer with leading zeros (0 <= a < 1000) and b is a single digit (1-9). Print a % b for each line.

## Approach

Parse a as the first 3 characters and b as the character at index 4. Use Mochi's % operator directly.
