# SPOJ FAST_BF2 - The BrainFast Processing! Challenge version

https://www.spoj.com/problems/FAST_BF2/

Given T lowercase strings, output YES if the string is a palindrome, NO otherwise.
Note: only Brainfuck is allowed on SPOJ; this is a Mochi implementation for our test suite.

## Approach

Compare characters symmetrically: s[i] vs s[n-1-i] for i in 0..n/2. If all match, palindrome.
