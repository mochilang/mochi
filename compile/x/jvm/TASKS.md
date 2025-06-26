# JVM Bytecode Backend Tasks for TPCH Q1

The JVM backend emits bytecode directly and currently lacks dataset query support.

- Represent tables with arrays of objects and generate loops for `group by` operations.
- Add helper methods in the generated class for `sum`, `avg` and `count`.
- Implement basic JSON output using string builders or a small library.
- Include Q1 golden tests under `tests/compiler/jvm` once compilation succeeds.
