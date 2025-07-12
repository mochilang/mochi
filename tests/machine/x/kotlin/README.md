# Kotlin Machine Outputs

This directory contains Kotlin source files generated from the Mochi programs in `tests/vm/valid`. They are produced by the Kotlin backend in `compiler/x/kotlin`.

## Compilation status

All 100 example programs compile successfully. Each has a `.kt` source file and a matching `.out` file with the runtime output.

## Remaining Tasks

- Improve the query translator to match the style of the reference implementations in `tests/human/x/kt`.
- Verify the generated code against more complex datasets such as `tests/dataset/tpc-h/q1.mochi`.
