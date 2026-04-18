# SPOJ BRAINF_K - Brainf_ck Interpreter

https://www.spoj.com/problems/BRAINF_K/

Implement a Brainfuck interpreter supporting the 7 commands `> < + - . [ ]` (no `,` input). Each test case is one line of BF source code. End of input at EOF. The tape is 30000 cells initialized to zero. Print the characters produced by `.`.

## Approach

Precompute matching bracket positions so `[` and `]` jumps are O(1). Use a string of printable ASCII chars (codes 32–126) to convert tape values to characters. Process commands in sequence, redirecting the instruction pointer on `[`/`]` based on the current cell value.
