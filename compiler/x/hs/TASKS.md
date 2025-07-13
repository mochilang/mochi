# Haskell Backend Progress

## Recent Updates (2025-07-13 05:05)
- Refactored runtime selection so programs that do not use `load`, `save` or `fetch` no longer import the `aeson` library.
- Generated `tpch_q1.mochi` for the first time. The program compiles to Haskell but requires `ghc` with `aeson` to run.

## Remaining Work
- [ ] Fully verify TPC-H query 1 output against reference results.
- [ ] Compile additional dataset queries (`q2`..`q22`).
- [ ] Improve pretty-printing of records to avoid verbose `Map` access.
