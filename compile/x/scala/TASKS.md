# Scala Backend Tasks for TPCH Q1

Scala code generation covers basic constructs but grouping logic is missing.

- Compile dataset queries to Scala collections using `.groupBy` and `.map` for aggregation.
- Define case classes for records like `lineitem` and compute aggregates with `sum`, `avg` and `size`.
- Emit JSON output with `scala.util.parsing.json` or a minimal serializer.
- Include a Q1 test in `tests/compiler/scala` once implemented.
