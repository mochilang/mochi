# Rust Backend

The Rust backend translates Mochi programs to Rust source code. It is used for experiments and to run solutions on systems where `rustc` is available.

## Unsupported features

The current implementation lacks support for:

- Nested function definitions.
- Automatic detection of mutable parameters.
- Map and string index helpers for generic collections.

These limitations cause some LeetCode programs to fail to compile.
