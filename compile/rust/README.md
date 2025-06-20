# Rust Backend

The Rust backend translates Mochi programs to Rust source code. It is used for experiments and to run solutions on systems where `rustc` is available.

## Unsupported features

The current implementation lacks support for:

- Advanced dataset queries such as grouping, sorting or pagination.
- Agent and stream declarations (`agent`, `on`, `emit`).
- Logic programming constructs (`fact`, `rule`, `query`).
- Data fetching and persistence expressions (`fetch`, `load`, `save`, `generate`).
- Package imports and the foreign function interface (`import`, `extern`).
- `test` and `expect` blocks are ignored.

These limitations cause some programs to fail to compile with the Rust backend.
