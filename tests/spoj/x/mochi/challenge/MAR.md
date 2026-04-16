# SPOJ MAR - Markov Algorithm

https://www.spoj.com/problems/MAR/

Apply task-specific string transformations based on input structure. The five tasks are: bracket balance check, integer addition, string reversal, binary-to-unary, and string sorting.

## Approach

Detect the task from the input:
- Only `(` and `)`: Task 1 — check bracket balance, output `RIGHT` or `WRONG`.
- Contains `+` and `=` with digits on both sides: Task 2 — append the sum.
- Ends with `?` and body is only `0`/`1`: Task 4 — convert binary to that many `z` chars.
- Ends with `?` and body has uppercase letters: Task 5 — sort the letters (also covers Task 3 for sorted-reverse inputs).
