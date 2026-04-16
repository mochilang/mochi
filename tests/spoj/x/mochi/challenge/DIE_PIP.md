# DIE_PIP - The Electronic Dice

https://www.spoj.com/problems/DIE_PIP/

Print a 3x3 binary pip grid for each die face value (1-6) read until EOF.
Blank line between consecutive dice.

## Approach

Each face's pip pattern is stored as a 9-character string. Decode each row
by indexing into the string and formatting with spaces.
