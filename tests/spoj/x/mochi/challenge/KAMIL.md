# SPOJ KAMIL - Kamil and His Letters

https://www.spoj.com/problems/KAMIL/

Kamil cannot distinguish between T, D, L, and F when speaking (they all sound the same to him).
Given 10 words, for each word count how many letters are in {T, D, L, F} and output 2^count —
the number of different words Kamil might have heard.

## Approach

For each word, count occurrences of T, D, L, F. Compute 2^count by repeated multiplication.
