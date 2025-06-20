# Rust Backend

The Rust backend translates Mochi programs to Rust source code. It is used for experiments and to run solutions on systems where `rustc` is available.

## Unsupported features

The current implementation lacks support for:

- Complex dataset queries using grouping, joins or pagination.
- Agent and stream declarations (`agent`, `on`, `emit`).
- Logic programming constructs (`fact`, `rule`, `query`).
- Data fetching and persistence expressions (`fetch`, `load`, `save`, `generate`).

These limitations cause some programs to fail to compile with the Rust backend.
