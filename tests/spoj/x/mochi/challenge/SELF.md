# SPOJ SELF - Source Code Replica

https://www.spoj.com/problems/SELF/

Read an integer n. If n > 0, print the program's own source code n times. If n < 0, print the reversed source |n| times. If n = 0, print nothing. Score = source code length (shorter is better).

## Approach

Classic quine construction with an `escStr` helper that escapes backslashes, double-quotes, and newlines for embedding in a Mochi string literal. The string `s` stores a template of the full program with `var s = ""` as a placeholder. At runtime, `src` is reconstructed by splicing the escaped value of `s` into the template at the right position (indices 407 and 409 bracket the empty placeholder string). For n < 0 the `rev` function reverses the entire source character by character before printing.
