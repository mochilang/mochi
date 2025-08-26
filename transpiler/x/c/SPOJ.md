# C SPOJ Transpiler Output

This directory stores C code generated from Mochi programs in `tests/spoj/x/mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-ctrans_update`.

Checklist of programs that currently transpile and run (5/10) - Last updated 2025-08-26 14:25 +0700:
| Index | Name | Status | Duration | Memory |
| ---: | --- | :---: | ---: | ---: |
| 1 | Life, the Universe, and Everything | ✓ | 196us | 1.7 MB |
| 2 | PRIME1 - Prime Generator | ✓ | 2.35ms | 2.0 MB |
| 3 | Substring Check (Bug Funny) | ✓ | 262us | 1.5 MB |
| 4 | SPOJ ONP - Transform the Expression | ✓ | 285us | 1.6 MB |
| 5 | SPOJ Problem 5: The Next Palindrome | ✓ | 386us | 1.5 MB |
| 6 | Simple Arithmetics |  |  |  |
| 7 | The Bulk |  |  |  |
| 8 | Complete the Sequence! |  |  |  |
| 9 | SPOJ Problem 9: Direct Visibility |  |  |  |
| 12 | MMIND - The Game of Master-Mind |  |  |  |
