# Rust Backend

The Rust backend translates Mochi programs to Rust source code. It is used for experiments and to run solutions on systems where `rustc` is available.

## Unsupported features

The current implementation lacks support for:

- Join clauses with explicit sides (`left`, `right`, `outer`).
- Agent and stream declarations (`agent`, `on`, `emit`).
- Intent handlers within agents (`intent`).
- Logic programming constructs (`fact`, `rule`, `query`).
- Data fetching and persistence expressions (`fetch`, `load`, `save`, `generate`).
- Package imports and the foreign function interface (`import`, `extern`).
- `model` blocks for LLM configuration.
- `test` and `expect` blocks are ignored.

These limitations cause some programs to fail to compile with the Rust backend.
