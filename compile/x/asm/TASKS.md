# Assembly backend tasks for TPCH Q1

The ASM backend relies on the C code generator and simply asks the host C compiler
to emit assembly. No dataset helpers are currently translated.

- Translate dataset queries to assembly by preserving the loops produced by the C backend.
- Provide assembly or C stubs implementing grouping and the aggregate helpers `sum`, `avg` and `count`.
- Represent query rows using static data sections and allocate lists dynamically.
- Implement a small runtime to print JSON results for tests.
- Once complete, add a golden test under `tests/compiler/asm` running `q1.mochi`.
