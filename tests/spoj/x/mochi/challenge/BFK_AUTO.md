# SPOJ BFK_AUTO - Automatic Brainfuck Code Generator

https://www.spoj.com/problems/BFK_AUTO/

Read a text file (printable ASCII, including line feeds) and output Brainfuck code that when executed prints exactly that text. Score is the total BF code length across all test files (shorter is better).

## Approach

Maintain the current cell value starting at 0. For each character, emit the difference as '+' or '-' repetitions followed by '.'. After each input line, emit the newline character (ASCII 10) the same way. This naive delta encoding is simple and correct; loop-based approaches would score better.
