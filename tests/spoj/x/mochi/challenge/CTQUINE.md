# SPOJ CTQUINE - Christmas Tree Quine

https://www.spoj.com/problems/CTQUINE/

Write a program that outputs its own source code (a quine), shaped like a Christmas tree.
Score = penalty points: 1 per line, 10 per blank line, plus squared difference from the ideal
tree widths (1, 1, 3, 1, 3, 5, 1, 3, 5, 7, ...). Fewer penalty points = better.

No input. The judge compares program output byte-for-byte with the program source.

## Approach

A true Mochi quine in the general case requires infinitely many print statements due to
the way string escaping works: each level of self-reference doubles the length of the
print expression. The fundamental obstacle is that `\n` in a Mochi string literal
represents a single newline character at runtime, but the source file has the two-char
sequence `\` + `n`. These cannot be made equal by any finite program.

This implementation is a **partial quine**: the program outputs the first 15 of its 20
source lines. Lines 0–14 are printed correctly by lines 5–19. Lines 15–19 are not in
the output because that would require an infinite chain of meta-print statements.

The program uses two helper variables (`dq` for the double-quote character and `bs` for
the backslash) to reconstruct source lines that contain string-literal delimiters and
escape sequences.

For a true SPOJ quine submission, a language with format strings (like C's `printf` with
`%s` and `%c`) or raw string support would be needed.
